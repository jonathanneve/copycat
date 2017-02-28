<?php

class Softsale_AdminLicenseController extends Am_Controller_Grid
{
    public function checkAdminPermissions(Admin $admin)
    {
        return $admin->hasPermission('softsale-licenses');
    }
    
    public function createGrid()
    {
        $ds = new Am_Query($this->getDi()->licenseTable);
        $ds->leftJoin('?_user', 'u', 'u.user_id=t.user_id');
        $ds->leftJoin('?_softsale_license_scheme', 's', 's.scheme_id=t.scheme_id');
        $ds->leftJoin('?_invoice', 'i', 'i.invoice_id=t.invoice_id');
        $ds->leftJoin('?_softsale_license_activation', 'a', 'a.license_id=t.license_id');
        $ds->addField('u.login', 'login');
        $ds->addField('s.title', 'scheme');
        $ds->addField("CONCAT(t.invoice_id, '/', IFNULL(i.public_id, ''))", 'invoice_num');
        $ds->addField('COUNT(a.license_activation_id)', 'activations_count');
        $grid = new Am_Grid_Editable('_license', 'Licenses', $ds, $this->_request, $this->view);
        $grid->setForm(array($this, 'createForm'));
        
        $grid->addField('login', ___('Username'))->addDecorator(
            new Am_Grid_Field_Decorator_Link('admin-users?_u_a=edit&_u_b={THIS_URL}&_u_id={user_id}'));
        $grid->addField('invoice_num', ___('Invoice'))->addDecorator(
            new Am_Grid_Field_Decorator_Link('admin-user-payments/index/user_id/{user_id}#invoice-{invoice_id}', '_top'));
        $grid->addField('scheme', 'Scheme')->addDecorator(
            new Am_Grid_Field_Decorator_Link('softsale/admin-scheme?_schemes_a=edit&_schemes_b={THIS_URL}&_schemes_id={scheme_id}'));
        $grid->addField('key', 'License Key');
        $grid->addField('activations_count', 'Activations')->addDecorator(
            new Am_Grid_Field_Decorator_DetailGrid('softsale/admin-license/activations/license_id/{license_id}', 'Activations')    
            );
        
        $grid->actionDelete('insert');
        $grid->actionDelete('delete');
        
        $grid->addCallback(Am_Grid_Editable::CB_RENDER_STATIC,
                array($this, 'renderStatic'));

        return $grid;
    }
    
    public function activationsAction()
    {
        $id = (int)$this->getParam('license_id');
        if (!$id) throw new Am_Exception_InputError('Empty id passed to ' . __METHOD__);
        
        $license = $this->getDi()->licenseTable->load($id);
        $this->view->license = $license;
        $this->view->activations = $license->getActivations();
        $this->view->display('admin/softsale/_activations.phtml');
    }
    
    public function addActivationAction()
    {
        $id = (int)$this->getParam('license_id');
        if (!$id) throw new Am_Exception_InputError('Empty id passed to ' . __METHOD__);
        $license = $this->getDi()->licenseTable->load($id);
        /* @var $license License */
        $response = $license->activate((array)$this->getParam('request'));
        if ($response->isValid())
        {
            return $this->activationsAction();
        } else {
            $ret = array('ok' => false, 'message' => $response->getMessage());
            $this->ajaxResponse($ret);
        }
    }
    
    public function disableActivationAction()
    {
        $id = (int)$this->getParam('license_id');
        if (!$id) throw new Am_Exception_InputError('Empty id passed to ' . __METHOD__);
        $license = $this->getDi()->licenseTable->load($id);
        /* @var $license License */
        $aid = (int)$this->getParam('license_activation_id');
        if (!$aid) throw new Am_Exception_InputError('Empty aid passed to ' . __METHOD__);
        $licenseActivation = $this->getDi()->licenseActivationTable->load($aid);
        $licenseActivation->disable();
        return $this->activationsAction();
    }
    
    public function createForm()
    {
        $form = new Am_Form_Admin;
        
        $form->addText('key', 'size=80')->setLabel('License Key');
        $form->addText('info', 'size=80')->setLabel('License Info');
        $form->addAdvCheckbox('is_disabled')->setLabel('Is Disabled?');
        
        return $form;
    }
    
    public function renderStatic(& $out, $grid)
    {
        $out .= <<<CUT
<script type="text/javascript">
jQuery(function($){
    $(".add-activation-button").live('click', function(event){
       var input = $(this).closest("tr").find(":input").serialize();
       $("#activation-message").hide();
       $.post(
           window.rootUrl + '/softsale/admin-license/add-activation',
           input, 
           function(data, textStatus){
                if (data.message) 
                    $("#activation-message").text("Activation error:" + data.message).slideDown();
                else {
                    $(".drid-detail-dialog").html(data);
                }
           }
       );
       return false;
    });
    $(".activation-disable").live('click', function(event){
       $.get(this.href,
           function(data, textStatus){
                if (data.message) 
                    $("#activation-message").text("Cannot disable:" + data.message).slideDown();
                else {
                    $(".drid-detail-dialog").html(data);
                }
           }
       );
       return false;
    });
});
</script>
<style type="text/css">
.div-flash-message  {
  width: 350px;
  margin-left: auto; margin-right: auto;
  padding: 1em;
  font-size: 12px;
  color: #454430;
  background: #ffffcf;
  border: solid 3px #545454;
  -moz-border-radius: 10px;
  -webkit-border-radius: 10px;
  border-radius: 10px;
  margin-bottom: 0.2em;
}
</style>
CUT;
    }
    
}