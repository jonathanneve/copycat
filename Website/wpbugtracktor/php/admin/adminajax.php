<?php
/**
 * This file handles all the wpbugtracktor admin only Ajax calls.
 */

if(!function_exists('wpbtAjaxJSONIssues')) {
    
    /**
     * Grabs the data for the data table and outputs it in JSON format
     * @global type $wpdb
     */
    function wpbtAjaxJSONIssues() {
        global $wpdb;

        wpBugTracktorCheckAdminPermissions();

        $issues = wpBugTracktorGetIssues('*', ";");

        /* Indexed column (used for fast and accurate table cardinality) */
        $sIndexColumn = "primkey";

        $sTable = $wpdb->prefix."wpbugtracktor_issues";
        $aColumns = array( 'primkey','title','description','type','status','reporter_id','reporter_email','reporter_ip','owner_id','project_id','project_component_id','severity_priority','version_reported','target_fix_for_milestone_id','tags','date_reported','date_last_modified','post_id','related_issue_ids','email_updates_to_these_users','attachments' );

        /* 
         * Paging
         */
        $sLimit = "";
        if ( isset( $_POST['iDisplayStart'] ) && $_POST['iDisplayLength'] != '-1' )
        {
                $sLimit = "LIMIT ".$wpdb->escape( $_POST['iDisplayStart'] ).", ".$wpdb->escape( $_POST['iDisplayLength'] );
        }


        /*
         * Ordering
         */
        if ( isset( $_POST['iSortCol_0'] ) )
        {
                $sOrder = "ORDER BY  ";
                for ( $i=0 ; $i<intval( $_POST['iSortingCols'] ) ; $i++ )
                {
                        if ( $_POST[ 'bSortable_'.intval($_POST['iSortCol_'.$i]) ] == "true" )
                        {
                                $sOrder .= $aColumns[ intval( $_POST['iSortCol_'.$i] ) ]."
                                        ".$wpdb->escape( $_POST['sSortDir_'.$i] ) .", ";
                        }
                }

                $sOrder = substr_replace( $sOrder, "", -2 );
                if ( $sOrder == "ORDER BY" )
                {
                        $sOrder = "";
                }
        }


        /* 
         * Filtering
         * NOTE this does not match the built-in DataTables filtering which does it
         * word by word on any field. It's possible to do here, but concerned about efficiency
         * on very large tables, and MySQL's regex functionality is very limited
         */
        $sWhere = "";
        if ( $_POST['sSearch'] != "" )
        {
                $sWhere = "WHERE (";
                for ( $i=0 ; $i<count($aColumns) ; $i++ )
                {
                        $sWhere .= $aColumns[$i]." LIKE '%".$wpdb->escape( $_POST['sSearch'] )."%' OR ";
                }
                $sWhere = substr_replace( $sWhere, "", -3 );
                $sWhere .= ')';
        }

        /* Individual column filtering */
        for ( $i=0 ; $i<count($aColumns) ; $i++ )
        {
                if ( $_POST['bSearchable_'.$i] == "true" && $_POST['sSearch_'.$i] != '' )
                {
                        if ( $sWhere == "" )
                        {
                                $sWhere = "WHERE ";
                        }
                        else
                        {
                                $sWhere .= " AND ";
                        }
                        $sWhere .= $aColumns[$i]." LIKE '%".$wpdb->escape($_POST['sSearch_'.$i])."%' ";
                }
        }


        /*
         * SQL queries
         * Get data to display
         */
        $sQuery = "
                SELECT SQL_CALC_FOUND_ROWS ".str_replace(" , ", " ", implode(", ", $aColumns))."
                FROM   $sTable
                $sWhere
                $sOrder
                $sLimit
        ";
        $rResult = $wpdb->get_results( $sQuery, ARRAY_N );

        /* Data set length after filtering */
        $sQuery = "
                SELECT FOUND_ROWS()
        ";
        $rResultFilterTotal = $wpdb->get_results( $sQuery, ARRAY_N );
        //$aResultFilterTotal = mysql_fetch_array($rResultFilterTotal);
        $iFilteredTotal = $rResultFilterTotal[0];

        /* Total data set length */
        $sQuery = "
                SELECT COUNT(".$sIndexColumn.")
                FROM   $sTable
        ";
        $rResultTotal = $wpdb->get_results( $sQuery, ARRAY_N );
        //$aResultTotal = mysql_fetch_array($rResultTotal);
        $aResultTotal = $rResultTotal;
        $iTotal = $aResultTotal[0];


        /*
         * Output
         */
        $output = NULL;
        $output = array(
                "sEcho" => intval($_POST['sEcho']),
                "iTotalRecords" => $iTotal,
                "iTotalDisplayRecords" => $iFilteredTotal,
                "aaData" => array()
        );


        foreach ($rResult as $aRow ) {
                $row = array();
                for ( $i=0 ; $i<count($aColumns) ; $i++ ) {

                    $row["DT_RowId"] = 'wpscid-'.$_POST['dbtable'].'-'.$aRow[0]; // Makes the ROW ID contain the table name, PRIMKEY, and column offset
                    switch ($aColumns[$i]) {

                        case 'primkey':
                            $row[] = '<div class="wpbt-edit-disabled">'.$aRow[$i].' <img src="'.plugins_url().'/wpbugtracktor/images/x_alt_24x24.png" style="cursor:pointer;margin:4px;float:left;" onclick="if ( confirm(\''.__('Are you sure you wish to delete this issue?', 'wpbugtracktor').'\') ) { jQuery.post(ajaxurl+\'?action=wpbt_delete_issue\', { primkey: '.$aRow[$i] .'}, function(data) { location.reload(); });  }" /> </div>';
                        break;

                        case 'reporter_id':
                            $class = 'wpbt-edit-user'; 
                            if($aRow[$i]==0) {
                                $row[] = '<div class="'.$class.'">'.__('Guest', 'wpbugtracktor').'</div>';
                            } else {
                                global $user_info;
                                $user_info = get_userdata($aRow[$i]);
                                $row[] = '<div class="'.$class.'">'.$user_info->display_name.'</div>';
                            }                    
                        break;

                        case 'type':
                            $class = 'wpbt-edit-type';
                            switch (intval($aRow[$i])) {
                                case 0:
                                    $type_name = __('Bug', 'wpbugtracktor');
                                break;
                                case 1:
                                    $type_name = __('Feature Request', 'wpbugtracktor');
                                break;
                                case 2:
                                    $type_name = __('Regression', 'wpbugtracktor');
                                break;
                                case 3:
                                    $type_name = __('Enhancement', 'wpbugtracktor');
                                break;
                                case 4:
                                    $type_name = __('Idea', 'wpbugtracktor');
                                break;                    
                            }                    
                            $row[] = '<div class="'.$class.'">'.$type_name.'</div>';     
                        break;

                        case 'severity_priority':
                            $class = 'wpbt-edit-severity';
                            switch (intval($aRow[$i])) {
                                case 0:
                                    $severity_name = __('Non-critical', 'wpbugtracktor');
                                break;
                                case 1:
                                    $severity_name = __('Low priority', 'wpbugtracktor');
                                break;
                                case 2:
                                    $severity_name = __('Medium', 'wpbugtracktor');
                                break;
                                case 3:
                                    $severity_name = __('Above Average', 'wpbugtracktor');
                                break;
                                case 4:
                                    $severity_name = __('Critical', 'wpbugtracktor');
                                break;    
                                case 5:
                                    $severity_name = __('Emergency', 'wpbugtracktor');
                                break;                      
                            }                    
                            $row[] = '<div class="'.$class.'">'.$severity_name.'</div>';     
                        break;            

                        case 'title':
                        case 'description':                    
                        case 'status':
                        case 'reporter_email':
                        case 'reporter_ip':
                        case 'tags':
                            $class = 'wpbt-edit';
                            $row[] = '<div class="'.$class.'">'.$aRow[$i].'</div>';
                        break;

                        case 'post_id':
                            $class = 'wpbt-edit-disabled';
                            $tempout = '<div class="'.$class.'">';

                            $wpbt_check_draft_page = get_page(intval($aRow[$i]));
                            if($wpbt_check_draft_page->post_status == 'draft') {
                                $tempout .= '<div style="font-size:0.75em;">'.__('Saved as draft.', 'wpbugtracktor').'</div> <button class="button-primary" onclick="wpbtPublish('.intval($aRow[$i]).');">'.__('Publish', 'wpbugtracktor').'</button>';
                            }
                            if($wpbt_check_draft_page->post_status == 'publish') {
                                $tempout .= '<div style="font-size:0.75em;">'.__('Published.', 'wpbugtracktor').'</div> <button class="button-seconary" onclick="wpbtUnpublish('.intval($aRow[$i]).');">'.__('Unpublish', 'wpbugtracktor').'</button>';
                            }                        

                             $tempout .= '<ul style="font-size:0.9em;margin:3px;"><li><a href="'.get_permalink(intval($aRow[$i])).'" target="_blank">'.__('View Page', 'wpbugtracktor').'</a></li>
                                    <li><a href="post.php?post='.intval($aRow[$i]).'&action=edit">'.__('Edit Page', 'wpbugtracktor').'</li>
                                    <!--<li><a href="">'.__('Edit Comments', 'wpbugtracktor').'</a></li>-->
                                    </ul></div>' ;
                             $row[] = $tempout;
                        break;            

                        case 'project_component_id':
                            $class = 'wpbt-edit-component';
                            if($aRow[$i]==0) {
                                $row[] = '<div class="'.$class.'">'.__('None', 'wpbugtracktor').'</div>';
                            } else {
                                $component_results = $wpdb->get_results("SELECT `title` FROM `{$wpdb->prefix}wpbugtracktor_components` WHERE `primkey`='".intval($aRow[$i])."';", ARRAY_A);
                                if(isset($component_results[0]['title'])) {
                                    $component_name = $component_results[0]['title'];
                                } else {
                                    $component_name = __('None', 'wpbugtracktor');
                                }
                                $row[] = '<div class="'.$class.'">'.$component_name.'</div>';
                            }   
                        break;     

                        case 'target_fix_for_milestone_id':
                        case 'version_reported':
                            $class = 'wpbt-edit-version';
                            if($aRow[$i]==0) {
                                $row[] = '<div class="'.$class.'">'.__('Unassigned', 'wpbugtracktor').'</div>';
                            } else {
                                $version_results = $wpdb->get_results("SELECT `title` FROM `{$wpdb->prefix}wpbugtracktor_milestones` WHERE `primkey`='".intval($aRow[$i])."';", ARRAY_A);
                                if(isset($version_results[0]['title'])) {
                                    $version_name = $version_results[0]['title'];
                                } else {
                                    $version_name = __('Unassigned', 'wpbugtracktor');
                                }
                                $row[] = '<div class="'.$class.'">'.$version_name.'</div>';
                            }   
                        break;                  

                        case 'owner_id':
                            $class = 'wpbt-edit-admin-user';
                            if($aRow[$i]==0) {
                                $row[] = '<div class="'.$class.'">'.__('Unassigned', 'wpbugtracktor').'</div>';
                            } else {
                                global $user_info;
                                $user_info = get_userdata($aRow[$i]);
                                $row[] = '<div class="'.$class.'">'.$user_info->display_name.'</div>';
                            }   
                        break;

                        case 'project_id':
                            $class = 'wpbt-edit-project';
                            $project_results = wpBugTracktorGetProjects($selection=' `title` ', ' WHERE `primkey`="'.intval($aRow[$i]).'";');
                            if(isset($project_results[0]['title'])) {
                                $project_name = $project_results[0]['title'];
                            } else {
                                $project_name = __('Unassigned', 'wpbugtracktor');
                            }                   
                            $row[] = '<div class="'.$class.'">'.$project_name.'</div>';
                        break;

                        default:
                            $class = 'wpbt-edit-disabled';
                            $row[] = '<div class="'.$class.'">'.$aRow[$i].'</div>';
                        break;

                    }




                }
                $output['aaData'][] = $row;
                $row = NULL;        
        }

        if(!headers_sent()) {
            @header('Content-type: application/json');
        }
        echo json_encode( $output );
        die();
    }
}

add_action( 'wp_ajax_wpbt_json_issues', 'wpbtAjaxJSONIssues' );

if(!function_exists('wpbtAjaxEditIssue')) {
    function wpbtAjaxEditIssue() {
        global $wpdb;

        wpBugTracktorCheckAdminPermissions();

        $value = $wpdb->escape($_POST['value']);
        $col = intval($_POST['column']);
        $row_raw = $_POST['row_id'];
        $row = intval(str_replace('wpscid--', '', $row_raw ));
        $sTable = $wpdb->prefix."wpbugtracktor_issues";
        $aColumns = array( 'primkey','title','description','type','status','reporter_id','reporter_email','reporter_ip','owner_id','project_id','project_component_id','severity_priority','version_reported','target_fix_for_milestone_id','tags','date_reported','date_last_modified','post_id','related_issue_ids','email_updates_to_these_users','attachments' );

        $wpdb->query("UPDATE `{$sTable}` SET `{$aColumns[$col]}`='{$value}' WHERE `primkey`='{$row}';");


        switch ($aColumns[$col]) {
            case 'type':
                switch (intval($value)) {
                    case 0:
                        $type_name = __('Bug', 'wpbugtracktor');
                    break;
                    case 1:
                        $type_name = __('Feature Request', 'wpbugtracktor');
                    break;
                    case 2:
                        $type_name = __('Regression', 'wpbugtracktor');
                    break;
                    case 3:
                        $type_name = __('Enhancement', 'wpbugtracktor');
                    break;
                    case 4:
                        $type_name = __('Idea', 'wpbugtracktor');
                    break;                    
                }   
                echo $type_name;
            break;
            case 'severity_priority':
                switch (intval($value)) {
                    case 0:
                        $severity_name = __('Non-critical', 'wpbugtracktor');
                    break;
                    case 1:
                        $severity_name = __('Low priority', 'wpbugtracktor');
                    break;
                    case 2:
                        $severity_name = __('Medium', 'wpbugtracktor');
                    break;
                    case 3:
                        $severity_name = __('Above Average', 'wpbugtracktor');
                    break;
                    case 4:
                        $severity_name = __('Critical', 'wpbugtracktor');
                    break;    
                    case 5:
                        $severity_name = __('Emergency', 'wpbugtracktor');
                    break;                    
                }   
                echo $severity_name;
            break;
            case 'target_fix_for_milestone_id':
            case 'version_reported':
                $version_results = $wpdb->get_results('SELECT `title` FROM `'.$wpdb->prefix.'wpbugtracktor_milestones` WHERE `primkey`="'.intval($value).'";', ARRAY_A);
                if(isset($version_results[0]['title'])) {
                    $version_name = $component_results[0]['title'];
                } else {
                    $version_name = __('Unassigned', 'wpbugtracktor');
                }
                echo $version_name ;
            break;
            case 'project_component_id':
                $component_results = $wpdb->get_results('SELECT `title` FROM `'.$wpdb->prefix.'wpbugtracktor_components` WHERE `primkey`="'.intval($value).'";', ARRAY_A);
                if(isset($component_results[0]['title'])) {
                    $component_name = $component_results[0]['title'];
                } else {
                    $component_name = __('None', 'wpbugtracktor');
                }
                echo $component_name ;
            break;
            case 'project_id':
                $project_results = wpBugTracktorGetProjects($selection=' `title` ', ' WHERE `primkey`="'.intval($value).'";');
                if(isset($project_results[0]['title'])) {
                    $project_name = $project_results[0]['title'];
                } else {
                    $project_name = __('Unassigned', 'wpbugtracktor');
                } 
                echo $project_name;
            break;
                    case 'reporter_id':
                    case 'owner_id':
                    if(intval($value)==0) {
                        $display_name  = __('Unassigned', 'wpbugtracktor');
                    } else {
                        global $user_info;
                        $user_info = get_userdata(intval($value));
                        $display_name = $user_info->display_name;
                    }  
                    echo $display_name;
                    break;
            default:
                echo $value;
            break;            
        } 
        die();
    }
   
}
add_action( 'wp_ajax_wpbt_edit_issue', 'wpbtAjaxEditIssue' );



if(!function_exists('wpbtAjaxDeleteComponent')) {
    function wpbtAjaxDeleteComponent() {
        global $wpdb;
        wpBugTracktorCheckAdminPermissions();

        $id = intval($_POST['primkey']);

        $sql = "DELETE FROM `{$wpdb->prefix}wpbugtracktor_components` WHERE `primkey`='{$id}'; ";

        if($sql!=null) {
            $wpdb->query($sql);
        } 
        die();
    }
}
add_action( 'wp_ajax_wpbt_delete_component', 'wpbtAjaxDeleteComponent' );



if(!function_exists('wpbtAjaxDeleteIssue')) {
    function wpbtAjaxDeleteIssue() {
        global $wpdb;
        wpBugTracktorCheckAdminPermissions();

        $id = intval($_POST['primkey']);

        $sql = "DELETE FROM `{$wpdb->prefix}wpbugtracktor_issues` WHERE `primkey`='{$id}'; ";

        if($sql!=null) {
            $wpdb->query($sql);
        } 
        die();
    }
}
add_action( 'wp_ajax_wpbt_delete_issue', 'wpbtAjaxDeleteIssue' );



if(!function_exists('wpbtAjaxDeleteProject')) {
    function wpbtAjaxDeleteProject() {
        global $wpdb;
        wpBugTracktorCheckAdminPermissions();

        $id = intval($_POST['primkey']);

        $sql = "DELETE FROM `{$wpdb->prefix}wpbugtracktor_projects` WHERE `primkey`='{$id}'; ";

        if($sql!=null) {
            $wpdb->query($sql);
        } 
        die();
    }
}
add_action( 'wp_ajax_wpbt_delete_project', 'wpbtAjaxDeleteProject' );


if(!function_exists('wpbtAjaxSaveComponent')) {
    /**
     * Save Components
     * 
     * @global type $wpdb
     */
    function wpbtAjaxSaveComponent() {

        global $wpdb;

        wpBugTracktorCheckAdminPermissions();

        @$wpbt_primkey = intval($_POST['wpbt_primkey']);
        $wpbt_projectid = intval($_POST['projectid']);
        $wpbt_title = $wpdb->escape($_POST['title']);
        $wpbt_desc = $wpdb->escape($_POST['desc']);

        if(@!isset($_POST['wpbt_primkey'])) {
            // Insert a new record
            $sql = "INSERT INTO `{$wpdb->prefix}wpbugtracktor_components` (`primkey`, `title`, `description`, `project_id`) VALUES (NULL, '{$wpbt_title}', '{$wpbt_desc}', '{$wpbt_projectid}');";
        } else {
            // Update an existing record
            $sql = "UPDATE `{$wpdb->prefix}wpbugtracktor_components` SET `title` = '{$wpbt_title}', `description` = '{$wpbt_desc}', `project_id` = '{$wpbt_projectid}' WHERE `primkey` = '{$wpbt_primkey}';";
        }
        $wpdb->query($sql);

        echo $wpdb->insert_id;
        die(); 
    }
}

add_action( 'wp_ajax_wpbt_save_component', 'wpbtAjaxSaveComponent' );



if(!function_exists('wpbtAjaxSaveMilestone')) {
    /**
     * Save Milestone
     * 
     * @global type $wpdb
     */
    function wpbtAjaxSaveMilestone() {
        global $wpdb;

        wpBugTracktorCheckAdminPermissions();

        @$wpbt_primkey = $_POST['wpbt_primkey'];
        $wpbt_version = $wpdb->escape($_POST['wpbt_version']);
        $wpbt_codename = $wpdb->escape($_POST['wpbt_codename']);
        $wpbt_desc = $wpdb->escape($_POST['wpbt_desc']);
        $wpbt_project = intval($_POST['wpbt_project']);
        $wpbt_isreleased = intval($_POST['wpbt_isreleased']);
        $wpbt_startdate = $wpdb->escape($_POST['wpbt_startdate']);
        $wpbt_releasedate = $wpdb->escape($_POST['wpbt_releasedate']);

        if(@!isset($_POST['wpbt_primkey'])) {
            // Insert a new record
            $sql = "INSERT INTO `{$wpdb->prefix}wpbugtracktor_milestones` (`primkey`, `title`, `version_number`, `description`, `project_id`, `is_released`, `start_date`, `release_date`) VALUES (NULL, '{$wpbt_codename}', '{$wpbt_version}', '{$wpbt_desc}', '{$wpbt_project}', '{$wpbt_isreleased}', '{$wpbt_startdate}', '{$wpbt_releasedate}');";
        } else {
            // Update an existing record
            $sql = "UPDATE `{$wpdb->prefix}wpbugtracktor_milestones` SET `title`='{$wpbt_codename}', `version_number`='{$wpbt_version}', `description`='{$wpbt_desc}', `project_id`='{$wpbt_project}', `is_released`='{$wpbt_isreleased}', `start_date`='{$wpbt_startdate}', `release_date`='{$wpbt_releasedate}' WHERE `primkey`='".intval($wpbt_primkey)."';";
        }
        $wpdb->query($sql);

        echo $wpdb->insert_id;   
        die();
    }
}
add_action( 'wp_ajax_wpbt_save_milestone', 'wpbtAjaxSaveMilestone' );





if(!function_exists('wpbtAjaxSaveProject')) {
    /**
     * Save Project
     * 
     * @global type $wpdb
     */
    function wpbtAjaxSaveProject() {
        global $wpdb;

        wpBugTracktorCheckAdminPermissions();

        $id_raw = $_POST['id'];
        $value = $_POST['value'];


        $sql = null;
        $to_be_replaced = array("title_", "description_", "prefix_", "owner_", "componentTitle_", "componentDescription_");
        $id = intval(str_replace($to_be_replaced, "", $id_raw));

        if (strpos($id_raw,'title') !== false) {
            $sql = "UPDATE `{$wpdb->prefix}wpbugtracktor_projects` SET `title`='$value' WHERE `primkey`='{$id}'; ";
        }  

        if (strpos($id_raw,'description') !== false) {
            $sql = "UPDATE `{$wpdb->prefix}wpbugtracktor_projects` SET `description`='$value' WHERE `primkey`='{$id}'; ";
        }  

        if (strpos($id_raw,'prefix') !== false) {
            $sql = "UPDATE `{$wpdb->prefix}wpbugtracktor_projects` SET `project_prefix`='$value' WHERE `primkey`='{$id}'; ";
        }      

        if (strpos($id_raw,'owner') !== false) {
            $sql = "UPDATE `{$wpdb->prefix}wpbugtracktor_projects` SET `owner_id`='$value' WHERE `primkey`='{$id}'; ";
            global $user_info;
            $user_info = get_userdata($value);    
            $value = $user_info->user_login;
        }   

        if (strpos($id_raw,'componentTitle') !== false) {
            $sql = "UPDATE `{$wpdb->prefix}wpbugtracktor_components` SET `title`='$value' WHERE `primkey`='{$id}'; ";
        } 

        if (strpos($id_raw,'componentDescription') !== false) {
            $sql = "UPDATE `{$wpdb->prefix}wpbugtracktor_components` SET `description`='$value' WHERE `primkey`='{$id}'; ";
        }  

        if($sql!=null) {
            $wpdb->query($sql);
        }


        echo $value;

        die(); 
    }
}
add_action( 'wp_ajax_wpbt_save_project', 'wpbtAjaxSaveProject' );



if(!function_exists('wpbtAjaxSaveSetting')) {
    /**
     * Save Setting
     * 
     * @global type $wpdb
     */
    function wpbtAjaxSaveSetting() {
        wpBugTracktorCheckAdminPermissions();

        $setting = $_POST['wpBugTracktorChangeSetting'];
        $settingValue = $_POST['wpBugTracktorChangeSettingValue'];

        $wpBugTracktorOptions = get_option('wpBugTracktorAdminOptions'); 

        $wpBugTracktorOptions[$setting] = $settingValue;

        update_option('wpBugTracktorAdminOptions', $wpBugTracktorOptions);
        die(); 
    }
}
add_action( 'wp_ajax_wpbt_save_setting', 'wpbtAjaxSaveSetting' );



if(!function_exists('wpbtAjaxSavePublish')) {
    /**
     * Publish or unPublish a page
     * 
     * @global type $wpdb
     */
    function wpbtAjaxSavePublish() {
        global $wpdb;

        wpBugTracktorCheckAdminPermissions();

        @$wpBugTracktorPublish = $wpdb->escape($_POST['wpBugTracktorPublish']);
        @$wpBugTracktorPublishPageId = intval($_POST['wpBugTracktorPublishPageId']);

        $my_post = array();
        $my_post['ID'] = $wpBugTracktorPublishPageId;
        if($wpBugTracktorPublish=='publish') {
            $my_post['post_status'] = 'publish';
        }
        if($wpBugTracktorPublish=='draft') {
            $my_post['post_status'] = 'draft';
        }                    
        wp_update_post( $my_post );   

        die(); 
    }
}
add_action( 'wp_ajax_wpbt_save_publish', 'wpbtAjaxSavePublish' );



?>