<?php

/**
 * Plugins are instantiated for each scheme with its own settings
 */
class Am_Softsale_Plugin_Standard extends Am_Softsale_Plugin_Abstract
{
    /**
     * @return Am_Softsale_Activator
     * @throws Am_Exception_Configuration
     */
    function createActivator()
    {
        return $this->_loadPlugin($this->getScheme()->activator_plugin_id, 'Am_Softsale_Activator_', 'activator');
    }
    
    /**
     * @return Am_Softsale_Keygen
     * @throws Am_Exception_Configuration
     */
    function createKeygen()
    {
        return $this->_loadPlugin($this->getScheme()->keygen_plugin_id, 'Am_Softsale_Keygen_', 'keygen');
    }
    
    protected function _loadPlugin($pl, $base_class, $type)
    {
        if (empty($pl))
            throw new Am_Exception_Configuration("Not configured $type [$pl] for plugin [{$this->getId()}]", 1);
        $class = $base_class . ucfirst(toCamelCase($pl));
        if (!class_exists($class, true))
            throw new Am_Exception_Configuration("Could not load $type [$pl] : [$class] for plugin [{$this->getId()}]", 2);
        return new $class($this, $this->getConfig());
    }
    
    public function isActivationAllowed(License $license, array $request, Am_Softsale_Activator_Response $response)
    {
        // check if activations are allowed
        $activations_count = $this->getDi()->licenseActivationTable->countActivations($license, 0);
        $activations_allowed = $this->getScheme()->activations_allowed;
        if ($activations_count >= $activations_allowed)
        {
            $response->setError(Am_Softsale_Activator_Response::ERROR_NO_SPARE_ACTIVATIONS, 
               ___("Sorry, you have already reached limit of activations (%d active, %d allowed)",
                   $activations_count, $activations_allowed));
            return false;
        }
        return true;
    }
    
    public function activateLicense(License $license, array $request)
    {
        $response = new Am_Softsale_Activator_Response;
        try {
            $request = $this->filterActivateRequest($request);
        } catch (Am_Exception $e) {
            $response->setError(Am_Softsale_Activator_Response::ERROR_INVALID_INPUT, $e->getMessage());
            return $response;
        }
        ksort($request);
        $response = new Am_Softsale_Activator_Response;

        if (!$this->isActivationUnique($license, $request, $response))
            return $response;
        
        if (!$this->isActivationAllowed($license, $request, $response))
            return $response;
        
        $response->setError(Am_Softsale_Activator_Response::ERROR_OTHER, "no response from activation plugin");
        $this->getScheme()->getActivator()->activateLicense($license, $request, $response);
        if ($response->isValid())
        {
            $activation = $this->getDi()->licenseActivationTable->saveActivation($license, $request, $response->getActivationCode());
        }
        return $response;
    }    
    
    function findAllowedActivationToDisable(License $license, $request, $response)
    {
        ksort($request);
        return $this->getDi()->licenseActivationTable->findFirstActive($license, $request);
    }
    
    function isReactivationAllowed(License $license, $request, $response)
    {
        $reactivations_made = $this->getDi()->licenseActivationTable->countActivations($license, 1);
        if ($reactivations_made < $this->getScheme()->reactivations_allowed)
            $response->setValid(null);
    }
    
    function isActivationUnique(License $license, $request, $response)
    {
        ksort($request);
        $activation = $this->getDi()->licenseActivationTable->findFirstDuplicate($license, $request);
        if ($activation && !$activation->is_disabled)
        {
            $response->setValid($activation->code);
            return false;
        }
        return true;
    }
    
    public function deactivateLicense(License $license, array $request)
    {
        $response = new Am_Softsale_Activator_Response;
        try {
            $request = $this->filterActivateRequest($request);
        } catch (Am_Exception_InputError $e) {
            $response->setError(Am_Softsale_Activator_Response::ERROR_INVALID_INPUT, $e->getMessage());
            return $response;
        }
        ksort($request);
        $response = new Am_Softsale_Activator_Response;
        $response->setError(Am_Softsale_Activator_Response::ERROR_NO_ACTIVATION_FOUND, 
                ___('Sorry, we have not found an activation record to deactivate'));
        $licenseActivation = $this->findAllowedActivationToDisable($license, $request, $response);
        if (!$licenseActivation)
            return $response;
        
        $response->setError(Am_Softsale_Activator_Response::ERROR_NO_REACTIVATION_ALLOWED, 
                ___('Sorry, license re-activation limit reached'));
        $this->isReactivationAllowed($license, $request, $response);
        if (!$response->isValid())
            return $response;
        
        $response->setError(Am_Softsale_Activator_Response::ERROR_OTHER, "no response from activation plugin");
        $this->getScheme()->getActivator()->deactivateLicense($license, $licenseActivation, $request, $response);
        if ($response->isValid())
        {
            $licenseActivation->disable($response->getCode());
        }
        return $response;
    }    
}