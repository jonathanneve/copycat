<?php

class License extends Am_Record_WithData 
{
    function renderKey($limit = 32)
    {
        $out = @$this->key;
        if (strlen($out)<$limit)
            return Am_Controller::escape($out);
        return sprintf("<span title='%s' onclick='this.innerHTML=this.title'>%s<sub> (open)</sub></span>",
            Am_Controller::escape($out),
            Am_Controller::escape(substr($out, 0, $limit))
        );
    }
    
    function setKey($key)
    {
        $this->key = $key;
    }
    function setInfo($info)
    {
        $this->info = $info;
    }
    function disable()
    {
        $this->updateQuick('is_disabled', 1);
        foreach ($this->getActivations() as $a)
            $a->disable();
    }
    /**
     * @return LicenseScheme
     */
    function getScheme()
    {
        return $this->getDi()->licenseSchemeTable->load($this->scheme_id);
    }
    
    function needActivation()
    {
        return $this->getScheme()->getActivator()->needActivation($this);
    }
    
    function isActivated()
    {
        return ($this->countActivations(0) > 0);
    }
    
    /**
     * @param array $request
     * @return Am_Softsale_Activator_Response
     */
    function activate(array $request)
    {
        return $this->getScheme()->getPlugin()->activateLicense($this, $request);
    }
    /**
     * @param array $request
     * @return Am_Softsale_Activator_Response
     */
    function deactivate(array $request)
    {
        return $this->getScheme()->getPlugin()->deactivateLicense($this, $request);
    }
    
    function getLocalExpiration()
    {
        $ret = '2099-12-31 23:59:59';
        if ($this->getScheme()->local_activation_valid > 0)
        {
            $ret = $this->getDi()->time + $this->getScheme()->local_activation_valid * 3600 * 24;
            $ret = sqlTime($ret);
        }
        return min($ret, $this->expires);
    }
    
    function countActivations($is_disabled = null)
    {
        return $this->getDi()->licenseActivationTable->countActivations($this, $is_disabled);
    }
    
    function getActivations()
    {
        return $this->getDi()->licenseActivationTable->findByLicenseId($this->pk());
    }
    
    function getNextCheck()
    {
        return max(0, 1 + strtotime($this->expires) - $this->getDi()->time);
    }
}

class LicenseTable extends Am_Table_WithData
{
    protected $_table = '?_softsale_license';
    protected $_key = 'license_id';
    protected $_recordClass = 'License';
    
    public function getForUser(User $user)
    {
        return $this->selectObjects("SELECT * FROM ?_softsale_license
            WHERE user_id=?d AND is_disabled = 0
            ORDER BY started DESC
        ", $user->pk());
    }
}