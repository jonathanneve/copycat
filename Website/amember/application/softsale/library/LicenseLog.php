<?php

class LicenseLog extends Am_Record_WithData 
{
}

class LicenseLogTable extends Am_Table_WithData
{
    protected $_table = '?_softsale_license_log';
    protected $_key = 'license_log_id';
    protected $_recordClass = 'LicenseLog';
    
    /**
     * @param string $event event class
     * @param mixed $info info about event
     * @param string|null $key License key used
     */
    function log($event, $info, $key = null)
    {
        $r = $this->createRecord();
        if (defined('AM_ADMIN') && AM_ADMIN)
        {
            $r->admin_id = $this->getDi()->authAdmin->getUserId();
        } else {
            $r->user_id = $this->getDi()->auth->getUserId();
        }
        $r->ip = $_SERVER['REMOTE_ADDR'];
        $r->dattm = $this->getDi()->sqlDateTime;
        $r->event = $event;
        if (!is_string($info))
            $info = json_encode($info, true);
        $r->info = $info;
        $r->key = $key;
        $r->insert(false);
    }
}