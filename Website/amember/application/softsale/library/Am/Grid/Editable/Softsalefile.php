<?php

class Am_Grid_Editable_Softsalefile extends Am_Grid_Editable_Content
{
    public function getTitle() {
        return ___('Software Downloads');
    }

    protected function createAdapter()
    {
        $q = new Am_Query(Am_Di::getInstance()->softsaleFileTable);
        $q->leftJoin('?_softsale_file_upload', 'sfu');
        $q->addField("COUNT(sfu.file_upload_id)", 'versions');
        return $q;
    }

    protected function initGridFields()
    {
        $this->addField('title', ___('Title'))->setRenderFunction(array($this, 'renderAccessTitle'));
        $this->addField('versions', ___('Versions'))
            ->setRenderFunction(array($this, 'renderVersion'))
            ->addDecorator(new Am_Grid_Field_Decorator_DetailGrid('softsale/admin-upload?file_id={file_id}', 'versions'));
        parent::initGridFields();
        $this->removeField('_link');
    }

    function renderVersion($record)
    {
        return sprintf('<td id="softsalefile-%d"><span id="softsalefile-%d-cnt">%d</span> (%s)</td>',
            $record->pk(),
            $record->pk(),
            $record->versions,
            ___('click to upload'));
    }

    function createForm()
    {
        $form = new Am_Form_Admin;
        $form->addText('title', array('class' => 'el-wide'))->setLabel(___('Title'))->addRule('required');
        $form->addText('desc', array('class' => 'el-wide'))->setLabel(___('Description'));
        $form->addAdvCheckbox('hide')->setLabel(___("Hide\n" . "do not display this item link in members area"));

        $form->addElement(new Am_Form_Element_ResourceAccess)->setName('_access')
            ->setLabel(___('Access Permissions'));

        return $form;
    }
}