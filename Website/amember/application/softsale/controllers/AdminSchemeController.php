<?php

/** implements "partial validation" of form - only "visible" fieldsets are validated */
class Am_Form_Admin_Softsale extends Am_Form_Admin
{
    public function validate()
    {
        $isSubmitted = false;
        foreach ($this->datasources as $ds) {
            if ($ds instanceof HTML_QuickForm2_DataSource_Submit) {
                $isSubmitted = true;
                break;
            }
        }
        if (!$isSubmitted) return false;
        $valid = true;
        foreach ($this as $child) 
        {
            if (!empty($child->isHidden)) continue; // <-- That is the CHANGE!
            $valid = $child->validate() && $valid;
        }
        return $valid;
    }
}

class Am_Grid_Action_SoftsaleShowCode extends Am_Grid_Action_Abstract
{
    protected $title = "Show Code";
    public function run()
    {
        echo "<div style='height: 20em; overflow: scroll; background-color: lightgrey;'>";
        $out = file_get_contents(__DIR__ . '/../library/Am/LicenseChecker.php');
        
        $ek = sha1($this->grid->getRecord()->local_hash_key);
        $out = preg_replace('#/\*ek\*/(.+)\*ek\*/#', "'$ek'", $out);
        $rv = str_replace("\n", "", var_export($this->grid->getRecord()->getBindTo(), true));
        $out = preg_replace('#/\*rv\*/(.+)\*rv\*/#', $rv, $out);
        $ch = " " . $this->grid->getRecord()->local_activation_valid . " ";
        $out = preg_replace('#/\*ch\*/(.+)\*ch\*/#', $ch, $out);        
        
        highlight_string($out);
        echo "</div>\n";
    }
}

class Softsale_AdminSchemeController extends Am_Controller_Grid
{
    public function checkAdminPermissions(Admin $admin)
    {
        return $admin->hasPermission('softsale-config');
    }

    public function createGrid()
    {
        $ds = new Am_Query($this->getDi()->licenseSchemeTable);
        $ds->leftJoin('?_softsale_license', 'l');
        $ds->addField('COUNT(l.license_id)', 'licenses_count');
        
        $grid = new Am_Grid_Editable('_schemes', ___('Licensing Schemes'), 
            $ds, $this->_request, $this->view, $this->getDi());
        $grid->addField('title', ___('Title'));
        $grid->addField('plugin_id', ___('Method'));
        $grid->addField('keygen_plugin_id', ___('Keygenerator'));
        $grid->addField('activator_plugin_id', ___('Activation'));
        //$grid->addField('_products', ___('Products'));
        $grid->addField('licenses_count', ___('Count of Licenses'));
        $grid->setForm(array($this, 'createForm'));

        $grid->setFormValueCallback('bind_to', array('RECORD', 'unserializeList'), array('RECORD', 'serializeList'));
        $grid->addCallback(Am_Grid_Editable::CB_VALUES_FROM_FORM, 
                array($this, 'valuesFromForm'));
        $grid->addCallback(Am_Grid_Editable::CB_VALUES_TO_FORM, 
                array($this, 'valuesToForm'));
        $grid->addCallback(Am_Grid_Editable::CB_AFTER_SAVE, 
                array($this, 'afterSave'));
        $grid->addCallback(Am_Grid_Editable::CB_AFTER_INSERT, 
                array($this, 'afterInsert'));
        $grid->actionAdd(new Am_Grid_Action_SoftsaleShowCode());
        return $grid;
    }
    
    public function afterInsert($values, LicenseScheme $record)
    {
        $et = $this->getDi()->emailTemplateTable->findFirstExact('softsale.new_license');
        $et0 = $this->getDi()->emailTemplateTable->createRecord($et->toArray());
        unset($et0->email_template_id);
        $et0->name = 'softsale_new_license_' . $record->pk();
        $et0->insert();
    }
    
    public function afterSave($values, LicenseScheme $record)
    {
        $record->setProducts($values['_product_ids']);
        foreach ($this->getPlugins($values) as $pl)
            $pl->schemeAfterSave($values, $record);
    }
    
    public function getPlugins($values)
    {
        $this->findPlugins($plugins, $pluginObjects, $supportedPlugins);
        $ret = array();
        foreach ($pluginObjects as $type => $pls)
        {
            if (empty($values[$type])) continue;
            $k = $values[$type];
            if (empty($pls[$k])) continue;
            $ret[] = $pls[$k];
        }
        return $ret;
    }
    
    public function valuesFromForm(& $values)
    {
        $k = 'softsale_new_license_' . $this->grid->getRecord()->pk();
        if (!empty($values[$k]) && $values[$k])
            $values['email_template'] = $k;
        else
            $values['email_template'] = null;
        foreach ($this->getPlugins($values) as $pl)
        {
            $pl->saveSchemeFormElements($values[$pl->getId()], $this->grid->getRecord());
        }
    }
    
    public function valuesToForm(& $values)
    {
        if (!empty($values['email_template']))
            $values['softsale_new_license_' . $this->grid->getRecord()->pk()] = 1;
        
        $values['_product_ids'] = $this->grid->getRecord()->getProducts();
        foreach ($this->getPlugins($values) as $pl)
            $values[$pl->getId()] = $pl->loadSchemeFormElements($this->grid->getRecord());
    }
    
    public function findPlugins( & $plugins, & $pluginObjects, & $supportedPlugins )
    {
        static $_pl, $_plo, $_spl;
        if (!empty($_pl) && !empty($_plo))
        {
            $plugins = $_pl; $pluginObjects = $_plo;
            $supportedPlugins = $_spl;
            return;
        }
        $types = array(
            'plugin_id' => array(
                'abstract' => 'Am_Softsale_Plugin_Abstract',
                'std' => 'Am_Softsale_Plugin_Standard',
                'check' => array('keygen_plugin_id', 'activator_plugin_id'),
            ),
            'keygen_plugin_id' => array(
                'abstract' => 'Am_Softsale_Keygen_Abstract',
                'std' => array('Am_Softsale_Keygen_PredefinedKeys',
                               'Am_Softsale_Keygen_Random'),
                'check' => array('activator_plugin_id'),
            ),
            'activator_plugin_id' => array(
                'abstract' => 'Am_Softsale_Activator_Abstract',
                'std' => 'Am_Softsale_Activator_Standard',
                'check' => array('manualactivator_plugin_id'),
            ),
            'manualactivator_plugin_id' => array(
                'abstract' => 'Am_Softsale_ManualActivator_Abstract',
                'std' => 'Am_Softsale_ManualActivator_Standard',
                'check' => array(),
            ),
        );
        $plugins = array();
        foreach ($types as $t => $type)
        {
            if ($t == 'plugin_id')
            {
                $plugins['plugin_id']['standard'] = 'Standard';
                $pluginObjects['plugin_id']['standard'] = $objstd = new Am_Softsale_Plugin_Standard($this->getDi(), array());
                foreach ($this->getDi()->plugins_softsale->loadEnabled()->getAllEnabled() as $pl)
                {
                    $plugins['plugin_id'][$pl->getId()] = $pl->getTitle();
                    $pluginObjects['plugin_id'][$pl->getId()] = $pl;
                }
            } else {
                $rpl = preg_replace('/Abstract$/', '', $type['abstract']);
                foreach (array_merge((array)$type['std'], amFindSuccessors($type['abstract'])) as $class)
                {
                    $obj = new $class($objstd, array());
                    $pluginObjects[ $t ][ $obj->getId() ] = $obj;
                    $plugins[$t][ $obj->getId() ] = $obj->getTitle();
                }
            }
        }
        foreach ($types as $t => $type)
        {
            foreach ($pluginObjects[$t] as $tt => $obj)
                foreach ($type['check'] as $check_t)
                {
                    $list = array_keys($plugins[$check_t]);
                    $list = $obj->filterSupportedPlugins($check_t, $list);
                    if ($check_t == 'activator_plugin_id')
                        $list[] = '';
                    $supportedPlugins[ $t ][ $tt ][ $check_t ] = $list;
                }
        }
        $_pl = $plugins;
        $_plo = $pluginObjects;
        $_spl = $supportedPlugins;
    }
    
    public function createForm()
    {
        $form = new Am_Form_Admin_Softsale();
        
        $plugins = $pluginObjects = array();
        $this->findPlugins($plugins, $pluginObjects, $supportedPlugins);
        
        $form->addText('title', array('class'=>'el-wide'))->setLabel(___('Title'))->addRule('required');
        $form->addTextarea('comment', array('class'=>'el-wide'))->setLabel(___('Comment'));
        $form->addAdvCheckbox('is_disabled')->setLabel(___('Disabled'));
        
        $sel = $form->addMagicSelect('_product_ids')->setLabel("Products\nusers who orders selected products\nwill receive these licenses");
        $sel->loadOptions($this->getDi()->productTable->getOptions());
        
        $sel = $form->addSelect('plugin_id', 'data-xx=1 id=plugin_id')
                    ->setLabel(___('Licensing Method'));
        $sel->addRule('required');
        $sel->loadOptions(array_merge_recursive($plugins['plugin_id']));
        
        $sel = $form->addSelect('keygen_plugin_id', 'data-xx=1 id=keygen_plugin_id')
                    ->setLabel(___('Key Generator'));
        $sel->addRule('required');
        $sel->loadOptions(array_merge_recursive(array('' => ''), $plugins['keygen_plugin_id']));
        
        $sel = $form->addAdvRadio('valid_to')->setLabel(___('Generated License Valid To'));
        $sel->loadOptions(array(
                '' => ___('Subscription Expiration (or next rebill date if subscription is recurring)'),
                Am_Period::MAX_SQL_DATE => ___('Lifetime (%s)', amDate(Am_Period::MAX_SQL_DATE)),
        ));
        
        if ($this->grid->getRecord()->pk())
            $em = $form->addElement('email_checkbox', 'softsale_new_license_' . $this->grid->getRecord()->pk());
        else
            $em = $form->addHtml()->setHtml('you can enable e-mail later when scheme is saved');
        $em->setLabel('E-Mail Generated License to Customer');
        
        $sel = $form->addSelect('activator_plugin_id', 'data-xx=1 id=activation_plugin_id')
                ->setLabel(___('Activation'));
        $sel->loadOptions(array_merge(array('' => ___('No activation')), $plugins['activator_plugin_id']));
        
        $form->addMagicSelect('bind_to', array('class' => 'hide-if-no-activator'))
            ->setLabel(___("Bind To\nactivation will be tied to the following installation parameters\nusually 1 choice is quite enough"))
            ->loadOptions(array(
                'ip' => 'Server IP',
                'url' => 'Installation URL',
                'domain' => 'Domain',
                'sdomain' => 'Secure Domain (if application can use 2 domains)',
                'hardware-id' => 'Hardware ID',
            ));

        $form->addInteger('activations_allowed', 'class=hide-if-no-activator')
            ->setLabel(___("Instances Limit\nHow many simultaneous bindings for license may exists (default - 1)"));
        
        $form->addInteger('reactivations_allowed', 'class=hide-if-no-activator')
            ->setLabel(___("Number of License Reactivations Allowed\nfor all instances (default - 0)"));

        $form->addInteger('local_activation_valid', 'class=hide-if-no-activator')
            ->setLabel(___("\"Call Home\" functionality - every .. days".
                "\nsoftware will periodically contact your site" . 
                "\nto check activation and license status" .
                "\nenter 0 to disable"));

        $sel = $form->addSelect('manualactivator_plugin_id', 'data-xx=1 id=manualactivator_plugin_id')
                ->setLabel(___('Manual Activation Method'));
        $sel->addRule('required');
        $sel->loadOptions(array_merge(array('' => ___('No activation')), $plugins['manualactivator_plugin_id']));
        
        $v = $form->getValue();
        $en = array(
            'plugin_id'           => @$v['plugin_id'],
            'keygen_plugin_id'    => @$v['keygen_plugin_id'], 
            'activator_plugin_id' => @$v['activator_plugin_id'],
            'manualactivator_plugin_id' => @$v['manualactivator_plugin_id']
        );
        // only add enabled plugins, so validation for not-enabled plugins does not create problems
        foreach ($pluginObjects as $type => $pls)
            foreach ($pls as $k => $pl)
            {
                $id = Am_Controller::escape($pl->getId());
                $fs = new Am_Form_Container_PrefixFieldset($pl->getId(), 
                        "class='fs-$type' id='fs-$type-$id' style='display: none;'");
                $fs->setLabel($pl->getTitle());
                $fs->isHidden = $en[$type] != $k;
                $pl->addSchemeFormElements($fs);
                if ($fs->count()) $form->addElement($fs);
            }
        
        $s = Am_Controller::getJson($supportedPlugins);
        $form->addScript()->setScript(<<<CUT
jQuery(function($){
   var supportedPlugins = $s;
   $("select[data-xx]").change(function(event){
      var val = $(this).val();
      var name = this.name;
      $("fieldset.fs-"+name).each(function(i,el){
         $(el).toggle('fs-' + name + '-' + val ==  el.id);
      });
      var supported = supportedPlugins[this.name] && supportedPlugins[this.name][val];
      if (supported)
      {
          $.each(supported, function(type, allowedValues){
                if (!allowedValues || !allowedValues.length) 
                {
                    $("select[data-xx][name='"+type+"'] option").prop("disabled", false); // allow all
                    return;
                }
                $("select[data-xx][name='"+type+"'] option").each(function(idx, el){
                    var found = false;
                    $.each(allowedValues, function(k,v){ if (v==el.value) found = true;});
                    $(el).prop('disabled', !found);
                });
                var sel = $("select[data-xx][name='"+type+"']");
                if (sel.find("option:disabled:selected").length)
                {
                    var enabled = sel.find("option:enabled");
                    if (enabled.size())
                        $(enabled[0]).prop('selected', true);
                }
                sel.change();
          });
      }
   }).change();
   $("select[name=activator_plugin_id]").change(function(){
       $(".hide-if-no-activator").closest(".row").toggle($(this).val() != '');
   }).change();
});
CUT
        );
        return $form;
    }
}