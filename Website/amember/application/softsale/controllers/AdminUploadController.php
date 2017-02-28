<?php

class Softsale_AdminUploadController extends Am_Controller_Grid
{
    public function checkAdminPermissions(Admin $admin) 
    {
        return $admin->hasPermission('softsale-config');
    }
    
    public function createForm()
    {
        $f = new Am_Form_Admin;
        $f->addText('version')->setLabel(___('Version'))->addRule('required');
        $f->addDate('date')->setLabel(___('Date'))->addRule('required');
        
        $f->addText('desc', array('class'=>'el-wide'))->setLabel(___('Description'));
        $f->addTextarea('details', array('class'=>'el-wide', 'rows'=>7))->setLabel(___('Details'));

        $maxFileSize = min(ini_get('post_max_size'), ini_get('upload_max_filesize'));
        $el = $f->addElement(new Am_Form_Element_Upload('path', array(), array('prefix' => 'softsale')))
                ->setLabel(___("File\n(max filesize %s)", $maxFileSize))->setId('form-path');
        $f->addHidden('file_id');
        
        // optional
        $f->addAdvCheckbox('hide')->setLabel(___('Hide'));
        return $f;
    }

    public function createGrid() 
    {
        $file = $this->getDi()->softsaleFileTable->load($this->_request->getInt('file_id'));
        /* @var $file SoftsaleFile */
        
        $ds = new Am_Query($this->getDi()->softsaleFileUploadTable);
        $ds->addWhere('file_id=?d', $file->file_id);
        $g = new Am_Grid_Editable('_uploads', ___("Versions for [%s]", $file->title), $ds, $this->_request, 
                $this->view);
        $g->addField('version', ___('Version'));
        $g->addField(new Am_Grid_Field_Date('date', ___('Date')))->setFormatDate();
        $g->addField('desc', ___('Description'));
        
        $g->actionGet('insert')->setTarget(null);
        $g->setForm(array($this, 'createForm'));

        $g->addCallback(Am_Grid_ReadOnly::CB_RENDER_TABLE, array($this, 'renderTable'));

        return $g;
    }

    public function renderTable(& $out, Am_Grid_Editable $grid)
    {
        $vars = $grid->getHiddenVars();
        $totalRecords = $vars['totalRecords'];
        $file_id = $this->getParam('file_id');

        $out .= <<<CUT
<script type="text/javascript">
$('#softsalefile-$file_id-cnt').html('$totalRecords');
</script>
CUT;
    }
}
