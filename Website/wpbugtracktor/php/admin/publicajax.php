<?php

if(!function_exists('wpbtAjaxSaveIssue')) {
    /**
     * Save an issue
     * 
     * @global type $wpdb
     */
    function wpbtAjaxSaveIssue() {

        global $wpdb, $current_user;

        @$title = $wpdb->escape($_POST['wpBugTracktorReportFormTitle']);
        @$description = $wpdb->escape($_POST['wpBugTracktorReportFormDescription']);
        @$type = intval($_POST['wpBugTracktorReportIssueType']);

        // User
        wp_get_current_user();
        if ( 0 == $current_user->ID ) {
            // Not logged in.
            $reporter_id = 0;
        } else {
            $reporter_id = $current_user->ID;
        }

        $reporter_email = $current_user->user_email; //$wpdb->escape($_POST['wpBugTracktorReportFormEmail']);

        // IP ADDRESS
        if ( @isset($_SERVER["REMOTE_ADDR"]) )    {
            $reporter_ip = $wpdb->escape($_SERVER["REMOTE_ADDR"]);
        } else if ( @isset($_SERVER["HTTP_X_FORWARDED_FOR"]) )    {
            $reporter_ip = $wpdb->escape($_SERVER["HTTP_X_FORWARDED_FOR"]);
        } else if ( @isset($_SERVER["HTTP_CLIENT_IP"]) )    {
            $reporter_ip = $wpdb->escape($_SERVER["HTTP_CLIENT_IP"]);
        } else {
            $reporter_ip = __('Unknown IP Address', 'wpbugtracktor');
        }

        $project_id = intval($_POST['wpBugTracktorReportIssueProject']);
        $projectresults = $wpdb->get_results("SELECT `owner_id` FROM `{$wpdb->prefix}wpbugtracktor_projects` WHERE `primkey`='{$project_id}';", ARRAY_A);

        if(@isset($projectresults[0]['owner_id'])) {
            $owner_id = $projectresults[0]['owner_id'];
        } else {
            $owner_id = 0;
        }

        if(@isset($_POST['wpBugTracktorReportSeverity'])) {
            $severity_priority = $wpdb->escape($_POST['wpBugTracktorReportSeverity']);
        } else {
            $severity_priority = 0;
        }

        if(@isset($_POST['wpBugTracktorReportVersionList'])) {
            $version_reported = intval($_POST['wpBugTracktorReportVersionList']);
        } else {
            $version_reported = 0;
        }

        if(@isset($_POST['wpBugTracktorReportMilestone'])) {
            wpBugTracktorCheckAdminPermissions();
            $target_fix_for_milestone_id = intval($_POST['wpBugTracktorReportMilestone']);
            $tags = $wpdb->escape($_POST['wpBugTracktorReportTags']);   
        } else {
            $target_fix_for_milestone_id = 0;
            $tags = '';   
        }

        $date_last_modified = date('Ymd');;

        if(@isset($_POST['wpBugTracktorReportComponentList'])) {
            $project_component_id = intval($_POST['wpBugTracktorReportComponentList']);
        } else {
            $project_component_id = 0;
        }

        if(@isset($_POST['wpbt_primkey'])) {
            @$wpbt_primkey = $_POST['wpbt_primkey'];
            $status = $_POST['wpBugTracktorReportIssueType'];
        } else {
   		/*
            $_POST['wpBugTracktorReportCaptcha'];

            if((@extension_loaded('gd') && @function_exists('gd_info'))) {
                if(session_id() == '') {
                    session_start();
                }

                @include_once(WP_PLUGIN_DIR.'/wpbugtracktor/php/securimage/securimage.php');
                @$securimage = new Securimage();

                if (@$securimage->check($_POST['wpBugTracktorReportCaptcha']) == false && (@extension_loaded('gd') && @function_exists('gd_info') && wpbtGdCheck())) {
                    echo '0';
                    exit();            
                } 
            } 
		*/   

            if( trim($title)=='' ||  trim($description)=='' ) {
                echo '0';
                exit();
            } else {

                $date_reported = date('Ymd');
                $status = __('Submitted (Pending Review)', 'wpbugtracktor');

                $wpBugTracktorOptions = get_option('wpBugTracktorAdminOptions'); 

                // Create our PAGE in draft mode in order to get the POST ID
                $my_post = array();
                $my_post['post_title'] = stripslashes($title);
                $my_post['post_type'] = 'page';
                $my_post['post_content'] = '';
                $my_post['post_status'] = 'draft';
                $my_post['post_author'] = 1;
                $my_post['post_parent'] = $wpBugTracktorOptions['mainpage'];

                // Insert the PAGE into the WP database
                $post_id = wp_insert_post( $my_post );	
                if($post_id==0) {
                        echo 0;
                        exit();
                }          

                $lastID = wpBugTracktorCreateIssue($title, $description, $type, $status, $reporter_id, $reporter_email, $reporter_ip, $owner_id, $project_id, $project_component_id, $severity_priority, $version_reported, $target_fix_for_milestone_id, $tags, $date_reported, $date_last_modified, $post_id);            

                // Now that we've inserted both the PAGE and the issuet, let's update and publish our post with the correct content
                $my_post = array();
                $my_post['ID'] = $post_id;
                $my_post['post_content'] = '[wpbugtracktor display=issues primkey='.$lastID.']';
                if($wpBugTracktorOptions['wpBugTracktorStatus']=='publish') {
                    $my_post['post_status'] = 'publish';
                }
                if($wpBugTracktorOptions['wpBugTracktorStatus']=='draft') {
                    $my_post['post_status'] = 'draft';
                }                    
                wp_update_post( $my_post );   

                echo get_permalink($post_id);        
            } 

        }

        die(); 
    }
}
add_action( 'wp_ajax_wpbt_save_issue', 'wpbtAjaxSaveIssue' );
add_action( 'wp_ajax_nopriv_wpbt_save_issue', 'wpbtAjaxSaveIssue' );



if(!function_exists('wpbtAjaxListComponents')) {
    /**
     * List components
     * 
     * @global type $wpdb
     */
    function wpbtAjaxListComponents() {
        global $wpdb;
       $output = '';

       $output .=  '<select class="wpBugTracktorFormSelect" name="wpBugTracktorReportComponentList" id="wpBugTracktorReportComponentList" ';
       if(@isset($_POST['projectid'])) {
           @$projectid = intval($_POST['projectid']);    
           $results = $wpdb->get_results("SELECT * FROM `{$wpdb->prefix}wpbugtracktor_components` WHERE `project_id`='{$projectid}' ;", ARRAY_A);

           if(@isset($results[0]['primkey'])) {
               $output .= '>';

               foreach ($results as $result) {
                   $output .= '<option value="'.$result['primkey'].'">'.$result['title'].'</option>';
               }


           } else {
                   $output .= ' style="display:none;">';
           }
        } else {
            $output .= ' style="display:none;">';
        }
        $output .= '</select>';

        echo $output;

        die(); 
    }
}
add_action( 'wp_ajax_wpbt_list_components', 'wpbtAjaxListComponents' );
add_action( 'wp_ajax_nopriv_wpbt_list_components', 'wpbtAjaxListComponents' );




if(!function_exists('wpbtAjaxListVersions')) {
    /**
     * List Versions
     * 
     * @global type $wpdb
     */
    function wpbtAjaxListVersions() {
        global $wpdb;

        $output = '';


        $output .=  '<select class="wpBugTracktorFormSelect" name="wpBugTracktorReportVersionList" id="wpBugTracktorReportVersionList" ';
        if(@isset($_POST['projectid'])) {
            @$projectid = intval($_POST['projectid']);    
            $results = $wpdb->get_results("SELECT * FROM `{$wpdb->prefix}wpbugtracktor_milestones` WHERE `project_id`='{$projectid}' ;", ARRAY_A);

            if(@isset($results[0]['primkey'])) {
                $output .= '>';

                foreach ($results as $result) {
                    $output .= '<option value="'.$result['primkey'].'">'.$result['version_number'].' ('.$result['title'].')</option>';
                }


            } else {
                    $output .= ' style="display:none;">';
            }
        } else {
            $output .= ' style="display:none;">';
        }
        $output .= '</select>';

        echo $output;    

        die(); 
    }
}
add_action( 'wp_ajax_wpbt_list_versions', 'wpbtAjaxListVersions' );
add_action( 'wp_ajax_nopriv_wpbt_list_versions', 'wpbtAjaxListVersions' );



?>