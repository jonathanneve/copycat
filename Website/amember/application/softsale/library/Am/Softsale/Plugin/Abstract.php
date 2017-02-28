<?php

/**
 * Plugin defines process of key creation, instantiates keygen and activation
 * plugins and can do a lot to modify softsale process
 */
abstract class Am_Softsale_Plugin_Abstract extends Am_Plugin
{
    protected $_configPrefix = 'softsale.';
    protected $_idPrefix = 'Am_Softsale_Plugin_';
    protected $_blobFields = array(); // config fields that must be read/written as BLOB
    
    protected $activator;
    protected $manualActivator;
    protected $keygen;
  
    /** @var LicenseScheme */
    protected $scheme;
    
    /**
     * @return Am_Softsale_Activator
     * @throws Am_Exception_Configuration
     */
    function getActivator()
    {
        if (!$this->activator)
            $this->activator = $this->createActivator ();
        return $this->activator;
    }
    /**
     * @return Am_Softsale_Activator_Abstract
     * @throws Am_Exception_Configuration
     */
    abstract function createActivator();
    
    /** @return Am_Softsale_Keygen */
    function getKeygen()
    {
        if (!$this->keygen)
            $this->keygen = $this->createKeygen ();
        return $this->keygen;
    }
    
    function hasActivator()
    {
        return (bool)$this->getScheme()->activator_plugin_id;
    }
    
    /**
     * @return Am_Softsale_Keygen_Abstract
     * @throws Am_Exception_Configuration
     */
    abstract function createKeygen();
    
       
    public function addSchemeFormElements(HTML_QuickForm2_Container $form)
    {
    }
    
    public function saveSchemeFormElements($values, LicenseScheme $record)
    {
        foreach ($values as $k => $v)
        {
            if (in_array($k, $this->_blobFields))
                $record->data()->setBlob($k,$v);
            else
                $record->data()->set($k,$v);
        }
    }
    
    public function loadSchemeFormElements(LicenseScheme $record)
    {
        $load = $record->data()->getAll();
        $ret = array();
        foreach ($load as $k => $v)
        {
            if (in_array($k, $this->_blobFields)) 
            {
                if ($v === Am_DataFieldStorage::BLOB_VALUE)
                    $ret[$k] = $record->data()->getBlob($k);
            } else {
                $ret[$k] = $v;
            }
        }
        return $ret;
    }

    /**
     * Plugin can override this function to run additional actions
     * on license scheme saving (for example store records into a 
     * separate table)
     * @param array $values
     * @param LicenseScheme $scheme
     */
    public function schemeAfterSave(array $values, LicenseScheme $scheme)
    {
    }
    
    /**
     * Plugin may filter supported plugins of $type and return
     * only plugins that are supported in conjuction with this plugin
     * By default return all passed $ids
     * @param type $type
     * @param type $id
     * @return array
     */
    public function filterSupportedPlugins($type, array $ids)
    {
        return $ids;
    }
    
    public function setScheme(LicenseScheme $scheme)
    {
        $this->scheme = $scheme;
        return $this;
    }
    /** @return LicenseScheme */
    public function getScheme()
    {
        return $this->scheme;
    }
    
    public function activateLicense(License $license, array $request)
    {
        throw new Am_Exception_NotImplemented("not implemented");
    }    
    public function deactivateLicense(License $license, array $request)
    {
        throw new Am_Exception_NotImplemented("not implemented");
    }
    /**
     * @throws Am_Exception_InputError if request has errors
     * @param array $request
     * @return array
     */
    public function filterActivateRequest(array $request)
    {
        $ret = array();
        foreach ($this->getScheme()->getBindTo() as $bind_param)
        {
            if (!array_key_exists($bind_param, $request))
                $ret[$bind_param] = null;
            else
                $ret[$bind_param] = preg_replace('/[^a-zA-Z0-9&%?.,;#:_-]+/', '', $request[$bind_param]);
            if (!strlen($ret[$bind_param]))
                throw new Am_Exception("Required activation parameter [$bind_param] not passed");
        }
        return $ret;
    }
}