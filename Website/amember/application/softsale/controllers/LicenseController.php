<?php

class Softsale_LicenseController extends Am_Controller
{
    public function preDispatch() 
    {
        $this->getDi()->user; // exception if not logged-in
    }
    
    public function indexAction()
    {
        $licenses = $this->getDi()->licenseTable->getForUser($this->getDi()->user);
        $this->view->licenses = $licenses;
        $this->view->hasActivation = false;
        foreach ($licenses as $l)
            if ($l->getScheme()->hasActivation())
            {
                $this->view->hasActivation = true;
                break;
            }
            
        $files = $this->getDi()->resourceAccessTable->getAllowedResources($this->getDi()->user, 
                SoftsaleFile::ACCESS_TYPE);
        
        foreach ($files as $k => $file)
        {
            if ($file->hide) { unset($files[$k]); continue; }
            if (!($file->getUploads(true))) { unset($files[$k]); continue; }
        }
        $this->view->files = $files;
        
        $this->view->display('softsale/licenses.phtml');
    }
    
    function renderActivationAction()
    {
        $id = $this->_request->getInt('id');
        $l = $this->getDi()->licenseTable->findFirstBy(array(
            'license_id' => $id,
            'user_id' => $this->getDi()->user->pk(),
        ));
        if (!$l)
            throw new Am_Exception_Db_NotFound("Incorrect license_id");
        /* @var $l License */
        if (!$l->needActivation())
            throw new Am_Exception_InputError("This license does not require activation");
        $manualActivator = $l->getScheme()->getActivator()->getManualActivator();
        if (!$manualActivator)
            throw new Am_Exception_InputError("This license does not support manual activation");
        return $manualActivator->render($l, $this->_request);
    }
}
