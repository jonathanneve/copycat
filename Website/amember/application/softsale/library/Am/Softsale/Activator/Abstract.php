<?php

abstract class Am_Softsale_Activator_Abstract extends Am_Softsale_Base
{
    abstract function activateLicense(License $license, array $request, Am_Softsale_Activator_Response $response);
    
    abstract function deactivateLicense(License $license, LicenseActivation $activation, array $request, 
            Am_Softsale_Activator_Response $response);
            
    
    /**
     * @return true if license needs activation
     */
    function needActivation(License $license)
    {
        return true;
    }
}