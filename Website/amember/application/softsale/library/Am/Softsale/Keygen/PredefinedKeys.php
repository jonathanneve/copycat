<?php

class Am_Softsale_Keygen_PredefinedKeys extends Am_Softsale_Keygen_Abstract
{
    protected $_blobFields = array('predefined-keys'); // long value
    
    protected function emailShortOfKeys($scheme)
    {
            $mail = new Am_Mail;
            $mail->toAdmin();
            $mail->setSubject('aMember SoftSale - Keygenerator problem');
            $mail->setBodyText(<<<CUT
   Dear Admin,

Your configured pre-defined license keys for Licensing Scheme #{$scheme->scheme_id} - [$scheme->title]
is close to run out. Please immediately visit License Scheme editing 
and add new keys.
--
    Your aMember SoftSale module
CUT
            );
            $mail->send();
    }
    
    protected function emailOutOfKeys($scheme)
    {
            $mail = new Am_Mail;
            $mail->toAdmin();
            $mail->setSubject('aMember SoftSale - Keygenerator FAILURE');
            $mail->setBodyText(<<<CUT
   Dear Admin,

Your configured pre-defined license keys for Licensing Scheme #{$scheme->scheme_id} - [$scheme->title]
is over. License key generation failed!
        
Please immediately visit License Scheme editing and add new keys.
--
    Your aMember SoftSale module
CUT
            );
            $mail->send();
    }
    
    public function generateLicense(InvoiceItem $item, Invoice $invoice, LicenseScheme $scheme, License $license)
    {
        $list = explode("\n", $scheme->data()->getBlob('predefined-keys'));
        while ($list && !($key = trim(array_shift($list))));
        if (!$key)
        {
            $this->emailOutOfKeys($scheme);
            return null;
        }
        $license->setKey($key);
        $scheme->data()->setBlob('predefined-keys', implode("\n", $list))->update();
        if (count($list) < 10)
        {
            $this->emailShortOfKeys($scheme);
        }
        return $license;
    }
    public function addSchemeFormElements(HTML_QuickForm2_Container $form)
    {
        $form->addTextarea('predefined-keys', 'rows=10 cols=40')
             ->setLabel("License Keys\ninsert keys into this list (one per line)")
             ->addRule('required');
    }
}
