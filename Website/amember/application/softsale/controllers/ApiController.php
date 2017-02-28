<?php

class Softsale_ApiController extends Zend_Controller_Action
{
    const OK = 'ok';
    const LICENSE_EMPTY = 'license_empty';
    const LICENSE_NOT_FOUND = 'license_not_found';
    const LICENSE_DISABLED = 'license_disabled';
    const LICENSE_EXPIRED = 'license_expired';
    const LICENSE_SERVER_ERROR = 'license_server_error';
    
    
    /** @return Am_Di */
    function getDi()
    {
        return $this->_invokeArgs['di'];
    }    
    function getJson($vars)
    {
        return json_encode($vars);//,JSON_FORCE_OBJECT);
    }
    function ajaxResponse($vars)
    {
        $this->getResponse()->setHeader('Content-type', 'application/json; charset=UTF-8');
        if (!empty($_GET['callback']))
        {
            if (preg_match('/\W/', $_GET['callback'])) {
                // if $_GET['callback'] contains a non-word character,
                // this could be an XSS attack.
                header('HTTP/1.1 400 Bad Request');
                exit();
            }
            $this->getResponse()->setBody(sprintf('%s(%s)', $_GET['callback'], $this->getJson($vars)));
        } else
            $this->getResponse()->setBody($this->getJson($vars));
    }
    function error($message, $code, array $params = array())
    {
        if (empty($params))
            $params['next_check'] = 120;
        $this->ajaxResponse(array(
            'message' => $message,
            'code' => $code,
        ) + $params);
    }
    /**
     * Expected input: 
     *   string key 
     */
    function checkLicenseAction()
    {
        $license = $this->checkFetchLicense();
        if (!$license instanceof License) return;
        
        $user = $this->getDi()->userTable->load($license->user_id);
        $scheme = $license->getScheme();
        return $this->ajaxResponse(array(
            'code' => 'ok',
            'message' => 'OK',
            //'username'  => $user->login,
            //'name'      => $user->getName(),
            //'email'     => $user->email,
            'scheme_id'         => $license->scheme_id,
            'scheme_title'      => $scheme->title,
            'license_expires'   => $license->expires,
            'next_check'        => $license->getNextCheck(),
        ));
    }
    
    /**
     * Expected input:
     *   string key
     *   string request[ip,root_url,domain,sdomain,hardware-id]
     * Return:
     *   code = 0 for success
     *   string activation_code
     */
    function activateAction()
    {
        $license = $this->checkFetchLicense();
        if (!$license instanceof License) return;

        $response = $license->activate((array)$this->_request->getParam('request'));
        if ($response->isValid())
        {
            return $this->ajaxResponse(array(
                'code'            => 'ok',
                'message'         => 'OK',
                'activation_code' => $response->getActivationCode(),
                'scheme_id'       => $license->scheme_id,
                'license_expires' => $license->expires,
                'next_check'      => $license->getNextCheck(),
            ));
        } else
            return $this->error($response->getMessage(), $response->getCode());
    }
    
    function deactivateAction()
    {
        $license = $this->checkFetchLicense();
        if (!$license instanceof License) return;
        
        $response = $license->deactivate((array)$this->_request->getParam('request'));
        if ($response->isValid())
            return $this->ajaxResponse(array(
                'code'            => 'ok',
                'message'         => 'OK',
                'activation_code' => $response->getActivationCode(),
                'scheme_id'       => $license->scheme_id,
                'license_expires' => $license->expires,
            ));
        else {
            return $this->error($response->getMessage(), $response->getCode());
        }
    }
    
    function checkActivationAction()
    {
        $license = $this->checkFetchLicense();
        if (!$license instanceof License) return;

        $activation = $this->getDi()->licenseActivationTable->findFirstActive($license, $this->_getParam('request'));
        
        if ($activation)
        {
            return $this->ajaxResponse(array(
                'code'            => 'ok',
                'message'         => 'OK',
                'activation_code' => $activation->code,
                'scheme_id'       => $license->scheme_id,
                'expires'         => $license->expires,
                'next_check'      => $license->getNextCheck(),
            ));
        } else {
            return $this->error("No activation found for this installation", 
                Am_Softsale_Activator_Response::ERROR_NO_ACTIVATION_FOUND);
        }
        
        
    }
    
    /** @return License|null */
    function checkFetchLicense()
    {
        $key = trim($this->_request->getParam('key'));
        if (!strlen($key)) 
            return $this->error('Empty License Key submitted', self::LICENSE_EMPTY);
        $license = $this->getDi()->licenseTable->findFirstByKey($key, false);
        if (!$license)
            return $this->error('License Key not found', self::LICENSE_NOT_FOUND);
        if ($license->is_disabled)
            return $this->error('License Key disabled', self::LICENSE_DISABLED);
        if ($license->expires < $this->getDi()->sqlDateTime)
        {
            $next_check = max(0, 7200 - strtotime($this->getDi()->sqlDateTime) 
                    - strtotime($license->expires)); // 2 hours grace period
            return $this->error('License expired', self::LICENSE_EXPIRED, array(
                'next_check' => $next_check,
                'grace_period' => $next_check > 0,
            ));
        }
        return $license;
    }
    
    public function _runAction($action)
    {
        try {
            return parent::_runAction($action);
        } catch (Exception $e) {
            $this->error("License server error", self::LICENSE_SERVER_ERROR);
        }
    }
}

