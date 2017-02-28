<?php

/**
 * Base class for softsale keygens and activators 
 */
abstract class Am_Softsale_Base
{
    /** @var Am_Softsale_Plugin_Abstract */
    protected $plugin;
    protected $config = array();
    protected $_blobFields = array();
    
    public function __construct(Am_Softsale_Plugin_Abstract $plugin, array $config) {
        $this->plugin = $plugin;
        $this->config = $config;
    }
    
    public function getId()
    {
        if (!preg_match('/\_([^_]+)$/', get_class($this), $regs))
            return strtolower(get_class($this));
        return str_replace('_', '-', fromCamelCase($regs[1]));
    }
    public function getTitle()
    {
        return ucfirst($this->getId());
    }

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
}