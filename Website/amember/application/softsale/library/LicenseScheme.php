<?php

class LicenseScheme extends Am_Record_WithData
{
    private $_pl;
    /**
     * @return Am_Softsale_Plugin
     */
    function getPlugin()
    {
        if (empty($this->_pl))
        {
            $this->getDi()->plugins_softsale->loadEnabled(); // load plugins
            $cl = 'Am_Softsale_Plugin_' . ucfirst(toCamelCase($this->plugin_id));
            if (!class_exists($cl, true))
                throw new Am_Exception_Configuration("Could not load softsale plugin for scheme#{$this->scheme_id}: {$this->plugin_id}");
            $this->_pl = new $cl($this->getDi(), (array)$this->getDi()->config->get('softsale.' . $this->plugin_id));
            $this->_pl->setScheme($this);
        }
        return $this->_pl;
    }
    
    /**
     * @return Am_Softsale_Activator
     * @throws Am_Exception_Configuration
     */
    function getActivator()
    {
        return $this->getPlugin()->getActivator();
    }
    
    /** @return Am_Softsale_Keygen */
    function getKeygen()
    {
        return $this->getPlugin()->getKeygen();
    }

    function hasActivation()
    {
        return $this->getPlugin()->hasActivator();
    }
    
    function generateLicense(InvoiceItem $item, Invoice $invoice)
    {
        for ($qty_i=0; $qty_i<$item->qty; $qty_i++)
        {
            $license = $this->getDi()->licenseTable->createRecord();

            $license->user_id = $invoice->user_id;
            $license->invoice_id = $invoice->pk();
            $license->invoice_item_id = $item->pk();
            $license->scheme_id = $this->pk();

            $license->created = $this->getDi()->sqlDateTime;
            $license->started = $this->getDi()->sqlDateTime;
            
            if ($this->valid_to == '2037-12-31')
            {
                $license->expires = $this->valid_to . ' 23:59:59';
            } elseif ($invoice->rebill_times) {
                if (!empty($invoice->rebill_date))
                    $license->expires = $invoice->rebill_date;
                else
                    $license->expires = $invoice->calculateRebillDate(
                       $invoice->getPaymentsCount() + (float)$invoice->first_total?0:1);
                $license->expires .=  ' 23:59:59';
            } else {
                $access = $this->getDi()->accessTable->findFirstByInvoiceItemId($item->pk());
                $license->expires = $access->expire_date;
            }
            $license = $this->getKeygen()->generateLicense($item, $invoice, $this, $license);
            if (!$license) 
            {
                $this->getDi()->licenseLogTable->log("license not generated", 
                    "Plugin decided to not issue license for Invoice: {$invoice->invoice_id}");
                return; // plugin decided to do not create license!
            }
            $this->getDi()->licenseLogTable->log("license generated", "Invoice: {$invoice->invoice_id} Key: $license->key");
            $license->save();
            
            if ($this->email_template)
            {
                $et = Am_Mail_Template::load($this->email_template, $invoice->getUser()->get('lang','en'));
                $et->setUser($invoice->getUser());
                $et->setInvoice($invoice);
                $et->setProduct($item->tryLoadProduct());
                $et->setScheme($scheme);
                $et->setLicense($license);
                $et->send($invoice->getUser());
            }
        }
    }
    
    function setProducts(array $products)
    {
        $this->getAdapter()->query("DELETE FROM ?_softsale_license_scheme_product 
            WHERE scheme_id=?d {AND product_id NOT IN (?a)}",
                $this->scheme_id, $products ? $products : DBSIMPLE_SKIP);
        if ($products) 
        {
            $vals = array();
            foreach ($products as $id)
                $vals[] = sprintf("(%d,%d)", $this->scheme_id, $id);
            $this->getAdapter()->query("INSERT IGNORE INTO ?_softsale_license_scheme_product
                (scheme_id, product_id)
                VALUES " . implode(", ", $vals));
        }
        return $this;
    }
    
    /** @return array of id# */
    function getProducts()
    {
        if (empty($this->scheme_id)) return array();
        return $this->getAdapter()->selectCol(
           "SELECT DISTINCT product_id 
            FROM ?_softsale_license_scheme_product
            WHERE scheme_id=?d", $this->scheme_id);
    }
    
    function getBindTo()
    {
        return $this->unserializeList($this->bind_to);
    }
}

class LicenseSchemeTable extends Am_Table_WithData
{
    protected $_table = '?_softsale_license_scheme';
    protected $_key = 'scheme_id';
    protected $_recordClass = 'LicenseScheme';
    protected $useCache = true;
    
    public function getForProductIds(array $product_ids)
    {
        $ret = array();
        $recs = $this->selectObjects("SELECT s.*, sp.product_id AS product_id 
            FROM ?_softsale_license_scheme_product sp
                RIGHT JOIN ?_softsale_license_scheme s USING (scheme_id)
            WHERE sp.product_id IN (?a)", $product_ids);
        foreach ($recs as $r)
        {
            $ret[$r->product_id][] = $r;
            unset($r->product_id);
        }
        return $ret;
    }
    
    public function preFetchCache()
    {
        $this->cache = array();
        foreach ($this->findBy() as $r)
            $this->cache[ $r->pk() ] = $r;
        return $this;
    }
    
    public function insert(array $values, $returnInserted = false) {
        if (empty($values['local_hash_key']))
            $values['local_hash_key'] = $this->getDi()->app->generateRandomString(64);
        return parent::insert($values, $returnInserted);
    }
}