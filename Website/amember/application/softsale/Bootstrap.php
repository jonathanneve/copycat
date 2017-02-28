<?php

/**
 * IMPLEMENT:
 *    SoftSale main menu in admin area
 *     -> License Schemes page (custom, looks like this)
 *           <h1>Licensing Schemes</h1>
 *           <h3>aMember License scheme <a href>edit</a><a href>delete</a><a href>disable</a></h3>   
 *           provides licenses for following products: 
 *              here is list of enabled products and javascript link to add
 *              more products (show magicselect? when link clicked?)
 *          <h3>Wordpress Plugin License scheme<a href>edit</a><a href>delete</a><a href>disable</a></h3>   
 *           provides licenses for following products: 
 *              here is list of enabled products and javascript link to add
 *              more products (show magicselect? when link clicked?)          
 * 
 *     -> edit link brings the scheme editor page
 *          look at db.xml -> license_scheme for field details
 *          + keygen and activation plugins must be able to ask for additional settings
 *            settings will be saved into scheme->data()->set(plugin_type.plugin_id.'config-key')
 *          for example "predefined list" plugin must display textarea to copy/paste
 *          list of plugins
 * 
 *     -> scheme must be impossible to delete if licenses are exist(!) link must be disabled
 *                  
 *     -> on product purchase (onAccessAfterInsert?) aMember must generate license keys
 *        for given scheme
 * 
 * 
 *     - implement REST controller for license activation
 *       -> software will contact aMember for license activation and send hostname,ip,other details
 *       -> aMember will check if activation allowed, run activation plugin and activate license
 *       -> LATER if temporary license is enabled in scheme, aMember will generate it too and include into response
 * 
 * TODO LATER:
 *    implement sample API to access it in PHP, [C, C++, Java?]
 *    (later) suggest to by-default provide files with Protect Content -> Files, but also implement
 *        Protect Content -> Software Files with version control
 */
class Bootstrap_Softsale extends Am_Module
{
    function init()
    {
        $this->getDi()->plugins_softsale = new Am_Plugins($this->getDi(),
            'softsale', dirname(__FILE__) . '/plugins',
            'Am_Softsale_Plugin_%s', '%s.%s'
        );
        $this->getDi()->plugins_softsale->setTitle('SoftSale Plugins');
        $this->getDi()->plugins->offsetSet('softsale', $this->getDi()->plugins_softsale);
        
        $this->getDi()->blocks->add(new Am_Block('admin/user/invoice/right', 
                "Licenses", 'invoice_licenses', $this, array($this, 'renderAdminInvoiceLicenses')));
        
    }
    
    public function renderAdminInvoiceLicenses(Am_View $view)
    {
        if (empty($view->invoice)) return;
        if (!$this->getDi()->authAdmin->getUser()->hasPermission('softsale-license')) return;
        $invoice = $view->invoice;
        /* @var $invoice Invoice */
        $licenses = $this->getDi()->licenseTable->findBy(array('invoice_id'=> $invoice->pk(),));
        if (!$licenses) return;
        $out = "\n<br /><b>Licenses:</b><br />\n";
        foreach ($licenses as $l)
        {
            $url = Am_Controller::escape(REL_ROOT_URL . '/softsale/admin-license?_license_a=edit&_license_id='.$l->license_id.'&_license_b='.$_SERVER['REQUEST_URI']);
            $out .= '<i>' . $l->getScheme()->title . '</i> : <b><a href="'.$url.'">' . $l->renderKey(16) . "</a></b><br />\n";
        }
        return $out;
    }
    
    public function onAdminMenu(Am_Event $event)
    {
        $menu = $event->getMenu();
        $menu->addPage(array(
            'id' => 'softsale',
            'uri' => '#',
            'label' => ___('Software'),
            'resource' => "softsale",
            'pages' => array_merge(array(
                array(
                    'id' => 'softsales-scheme',
                    'controller' => 'admin-scheme',
                    'module' => 'softsale',
                    'label' => ___("Licensing Schemes"),
                    'resource' => "softsale-config",
                ),
                array(
                    'id' => 'softsales-license',
                    'controller' => 'admin-license',
                    'module' => 'softsale',
                    'label' => ___("Licenses"),
                    'resource' => "softsale-license",
                ),
                array(
                    'id' => 'content-softsalefile',
                    'uri' => REL_ROOT_URL . '/default/admin-content/p/softsalefile/index',
                    'label' => ___('Software Download'),
                    'resource' => 'softsale-config',
                ),
        ))));
    }
    
    public function onInvoiceStarted(Am_Event_InvoiceStarted $event)
    {
        $invoice = $event->getInvoice();
        $ids = array();
        foreach ($invoice->getItems() as $item)
            if ($item->item_type == 'product')
                $ids[] = $item->item_id;
        $schemes = $this->getDi()->licenseSchemeTable->getForProductIds($ids);
        foreach ($invoice->getItems() as $item)
        {
            if (empty($schemes[$item->item_id])) continue;
            foreach ($schemes[$item->item_id] as $scheme)
            {
                /* @var $scheme LicenseScheme */
                $scheme->generateLicense($item, $invoice);
            }
        }
    }
    
    public function onInvoiceAfterDelete(Am_Event $event)
    {
        $invoice_id = $event->getInvoice()->pk();
        $licenses = $this->getDi()->licenseTable->findByInvoiceId($invoice_id);
        foreach ($licenses as $l)
        {
            $this->getDi()->licenseLogTable->log("license deleted", "license [$l->key] deleted along with invoice [$invoice_id]");
            $l->disable();
        }
    }
    
    public function onPaymentAfterInsert(Am_Event_PaymentAfterInsert $event)
    {
        $invoice = $event->getInvoice();
        $pc = $invoice->getPaymentsCount();
        if ((double)$invoice->first_total && ($pc == 1)) return;
        if (!(double)$invoice->first_total && ($pc == 0)) return; //?
        // we handle only rebill here, first payment handled in onInvoiceStarted
        $ids = array();
        foreach ($invoice->getItems() as $item)
            if ($item->item_type == 'product')
                $ids[] = $item->item_id;
        $schemes = $this->getDi()->licenseSchemeTable->getForProductIds($ids);
        foreach ($invoice->getItems() as $item)
        {
            if (empty($schemes[$item->item_id])) continue;
            foreach ($schemes[$item->item_id] as $scheme)
            {
                if ($scheme->valid_to != '') continue; // expiration date not depends on rebill
                /* @var $scheme LicenseScheme */
                $scheme->generateLicense($item, $invoice);
            }
        }
    }
    
    function onRefundAfterInsert(Am_Event $event)
    {
        $invoice = $event->getInvoice();
        $licenses = $this->getDi()->licenseTable->findByInvoiceId($invoice->invoice_id);
        foreach ($licenses as $l)
        {
            $this->getDi()->licenseLogTable->log("license deleted", "license [$l->key] deleted with refund of invoice [$invoice_id]");
            $l->disable();
        }
    }

    function onGetPermissionsList(Am_Event $event)
    {
        $event->addReturn("Can manage software licensing schemes", 'softsale-config');
        $event->addReturn("Can manage software licenses", "softsale-license");
    }

    function onUserMenu(Am_Event $event)
    {
        $event->getMenu()->addPage(
            array(
                'id' => 'license',
                'controller' => 'license',
                'module' => 'softsale',
                'label' => ___('Licenses'),
                'order' => 800,
            )
        );
    }
    
    public function _initSetupForm(Am_Form_Setup $form)
    {
        $form->addElement('advcheckbox', 'softsale.show_activation_details')
            ->setLabel(___("Show Activation Details\nin user area"));

    }    
    
    public function getReadme()
    {
        $root = ROOT_URL . '/example';
        return <<<CUT
CUT;
    }
    
    function onGetResourceTypes(Am_Event $event)
    {
        if ($event->getType() == ResourceAccess::USER_VISIBLE_TYPES) {
            $return = $event->getReturn();
            array_push($return, SoftsaleFile::ACCESS_TYPE);
            $event->setReturn($return);
        }
    }

    function onInitAccessTables(Am_Event $event)
    {
        $event->getRegistry()->registerAccessTable($this->getDi()->softsaleFileTable);
    }
}