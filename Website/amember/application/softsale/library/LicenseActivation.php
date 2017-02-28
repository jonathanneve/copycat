<?php

class LicenseActivation extends Am_Record_WithData 
{
    function disable($deactivationCode = null)
    {
        $this->updateQuick(array(
            'is_disabled' => 1,
            'disabled_dattm' => $this->getDi()->sqlDateTime,
        ));
    }
    
    function renderRequest()
    {
        $request = json_decode($this->request, true);
        if (count($request) == 1)
            return Am_Controller::escape(current($request));
        else
            return Am_Controller::escape(implode(",", $request));
    }
}

class LicenseActivationTable extends Am_Table_WithData
{
    protected $_table = '?_softsale_license_activation';
    protected $_key = 'license_activation_id';
    protected $_recordClass = 'LicenseActivation';
    
    
    function countActivations(License $license, $is_disabled = null)
    {
        return $this->_db->selectCell("SELECT COUNT(*) 
            FROM {$this->_table} 
            WHERE license_id=?d {AND is_disabled = ?d}
        ", $license->pk(), $is_disabled === null ? DBSIMPLE_SKIP : $is_disabled);
    }
    
    function saveActivation(License $license, array $request, $activationCode)
    {
        $this->insert(array(
            'dattm' => $this->getDi()->sqlDateTime,
            'license_id' => $license->pk(),
            'request' => $this->encodeRequest($request),
            'code' => $activationCode,
        ));
    }
    
    function findFirstActive(License $license, array $request)
    {
        return $this->findFirstBy(array(
            'license_id' => $license->pk(),
            'is_disabled' => 0,
            'request' => $this->encodeRequest($request),
        ));
    }
    
    function findFirstDuplicate(License $license, array $request)
    {
        return $this->findFirstBy(array(
            'license_id' => $license->pk(),
            'is_disabled' => 0,
            'request' => $this->encodeRequest($request),
        ));
    }
    
    function encodeRequest(array $request)
    {
        ksort($request);
        return json_encode($request);
    }
}