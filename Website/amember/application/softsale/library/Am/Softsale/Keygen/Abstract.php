<?php

abstract class Am_Softsale_Keygen_Abstract extends Am_Softsale_Base
{
    /** @var Am_Softsale_Plugin_Abstract */
    protected $plugin;
    protected $config = array();
    
    public function __construct(Am_Softsale_Plugin_Abstract $plugin, array $config) {
        $this->plugin = $plugin;
        $this->config = $config;
    }
    
    /**
     * this method must generate license key based on information
     * from $access, $access->getInvoice(), $access->getUser(), and  $scheme
     * and set license key using $license->setLicense($key) method
     * $license object will be ->save()'d after succesful call to this method
     * 
     * In case of errors, this method must raise exception
     * 
     * This method may write additional information like $license->data()->set('my-custom-info')
     * it will be saved and available later
     * @return License|null it is NECESSARY to return license object or null if no license must be generated!
     */
    abstract function generateLicense(InvoiceItem $item, Invoice $invoice, LicenseScheme $scheme, License $license);
}