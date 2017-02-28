<?php
class Am_Softsale_Keygen_Random extends Am_Softsale_Keygen_Abstract
{
    public function generateLicense(InvoiceItem $item, Invoice $invoice, LicenseScheme $scheme, License $license)
    {
        $len = $license->getScheme()->data()->get('key_length');
        $di = $invoice->getDi();
        if ($len <= 0) $len = 10;
        do {
            $key = $di->app->generateRandomString($len, 'QWERTYUPASDFGHJKLZXCVBNM123456789');
            $key = $license->getScheme()->data()->get('key_prefix') . $key;
        } while ($di->licenseTable->findFirstByKey($key));
        $license->setKey($key);
        return $license;
    }
    
    public function addSchemeFormElements(HTML_QuickForm2_Container $form)
    {
        $form->addInteger('key_length', 'size=10')
             ->setLabel("Keycode Length (default - 10)")
             ->addRule('required');

        $form->addText('key_prefix', 'size=15 maxlength=10')
             ->setLabel("Keycode Prefix")
             ->addRule('regex', 'Only characters [a-zA-Z-_] are allowed', '/^[a-zA-Z_-]*$/');
    }
}
