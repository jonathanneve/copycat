<?php

class Am_Softsale_Activator_Response 
{
    const VALID = 'ok';
    const ERROR_INVALID_INPUT = 'invalid_input';
    const ERROR_NO_SPARE_ACTIVATIONS = 'no_spare_activations';
    
    const ERROR_NO_ACTIVATION_FOUND = 'no_activation_found';
    const ERROR_NO_REACTIVATION_ALLOWED = 'no_reactivation_allowed';
    
    const ERROR_NO_RESPONSE = 'no_response';
    const ERROR_OTHER = 'other_error';
    
    protected $code = self::ERROR_NO_RESPONSE;
    protected $activationCode;
    protected $message;
   
    function setValid($activationCode) 
    {
        $this->activationCode = $activationCode;
        $this->code = self::VALID;
        $this->message = null;
        return $this;
    }
    
    function setError($code, $message)
    {
        $this->code = $code;
        $this->message = $message;
        $this->activationCode = null;
        return $this;
    }
    
    function isValid() { return $this->code == self::VALID; }
    function getCode() { return $this->code; }
    function getActivationCode() { return $this->activationCode; }
    function getMessage() { return $this->message; }
}