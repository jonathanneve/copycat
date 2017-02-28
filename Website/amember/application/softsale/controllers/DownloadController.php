<?php

class Softsale_DownloadController extends Am_Controller
{
    public function preDispatch() 
    {
        return $this->getDi()->user;
    }
    
    public function indexAction()
    {
        $file = $this->getDi()->softsaleFileTable->load($this->_request->getInt('file_id'));
        /* @var $file SoftsaleFile */
        if (!$file->hasAccess($this->getDi()->user))
            throw new Am_Exception_InputError("Access denied reason #1");
        if ($file->hide)
            throw new Am_Exception_InputError("Access denied reason #2");
        $upload = $this->getDi()->softsaleFileUploadTable->load($this->_request->getInt('file_upload_id'));
        if ($upload->file_id != $file->pk())
            throw new Am_Exception_InputError("Access denied reason #3");
        if ($upload->hide)
            throw new Am_Exception_InputError("Access denied reason #4");

        /* @var $upload SoftsaleFileUpload */
        if ($path = $upload->getFullPath())
        {
            @ini_set('zlib.output_compression', 'Off'); // for IE
            $this->_helper->sendFile($path, $upload->getMime(), 
                array(
                    //'cache'=>array('max-age'=>3600),
                    'filename' => $upload->getDisplayFilename(),
            ));
        } else
            $this->redirectLocation($upload->getProtectedUrl(600));
    }
}
