<?php

class Am_Softsale_Activator_Standard extends Am_Softsale_Activator_Abstract
{
    public function addSchemeFormElements(HTML_QuickForm2_Container $form)
    {
    }

    function activateLicense(License $license, array $request, Am_Softsale_Activator_Response $response)
    {
        ksort($request);
        $text = $license->getLocalExpiration(); // local activation expires Y-m-d H:i:s
        $sign = sha1($sign_text = 
                    $text .
                    implode('=', array_values($request)) .
                    $license->key .
                    $license->getScheme()->local_hash_key);
        $text =  $sign . '|' . $text;
        $code = base64_encode($text);
        $code = preg_replace_callback('/^(.+?)(=*)$/', array($this, '_replaceEq'), $code);
        $code = 'AA' . $code; // activation string prefix - amember activation
        
        $response->setValid($code);
    }
    
    function deactivateLicense(License $license, LicenseActivation $activation, array $request, 
            Am_Softsale_Activator_Response $response)
    {
        $response->setValid(null);
    }
    
    
    function _replaceEq($regs)
    {
        return $regs[1] . strlen(@$regs[2]);
    }

    public function isCompatibleWithKeygen(Am_Softsale_Keygen $keygen)
    {
        return true;
    }
    
    public function needActivation(License $license)
    {
        return true;
    }
}