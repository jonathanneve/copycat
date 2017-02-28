<?php


if(!function_exists('wpBugTracktorCreateIssue')) {
    function wpBugTracktorCreateIssue($title, $description, $type, $status, $reporter_id, $reporter_email, $reporter_ip, $owner_id, $project_id, $project_component_id, $severity_priority, $version_reported, $target_fix_for_milestone_id, $tags, $date_reported, $date_last_modified, $post_id=0, $related_issue_ids='', $email_updates_to_these_users='', $attachments='') {
        global $wpdb;
        $wpdb->query("INSERT INTO `{$wpdb->prefix}wpbugtracktor_issues` (`primkey`, `title`, `description`, `type`, `status`, `reporter_id`, `reporter_email`, `reporter_ip`, `owner_id`, `project_id`, `project_component_id`, `severity_priority`, `version_reported`, `target_fix_for_milestone_id`, `tags`, `date_reported`, `date_last_modified`, `post_id`, `related_issue_ids`, `email_updates_to_these_users`, `attachments`) VALUES (NULL, '{$title}', '{$description}', '{$type}', '{$status}', '{$reporter_id}', '{$reporter_email}', '{$reporter_ip}', '{$owner_id}', '{$project_id}', '{$project_component_id}', '{$severity_priority}', '{$version_reported}', '{$target_fix_for_milestone_id}', '{$tags}', '{$date_reported}', '{$date_last_modified}', '{$post_id}', '{$related_issue_ids}', '{$email_updates_to_these_users}', '{$attachments}');");
        return $wpdb->insert_id;            
        
    }
}

if(!function_exists('wpBugTracktorGetIssues')) {
    function wpBugTracktorGetIssues($selection='*', $whereclause=NULL) {
        global $wpdb;
        return $wpdb->get_results("SELECT {$selection} FROM `{$wpdb->prefix}wpbugtracktor_issues` {$whereclause};", ARRAY_A);
    }
}

if(!function_exists('wpBugTracktorIsPublishedPage')) {
    /**
     *
     * @global object $wpdb
     * @param type $issue_id
     * @return boolean Returns TRUE if the issue is published, returns FALSE if its a draft or couldn't be found
     */
    function wpBugTracktorIsPublishedPage($issue_id) {
        global $wpdb;
        $result = $wpdb->get_results("SELECT `post_id` FROM `{$wpdb->prefix}wpbugtracktor_issues` WHERE `primkey`='{$issue_id}'; ", ARRAY_A);
        if(isset($result[0]['post_id'])) {
            $wpsc_check_draft_page = get_page($result[0]['post_id']);  
            if($wpsc_check_draft_page->post_status == 'publish') {
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }
}

if(!function_exists('wpBugTracktorGetComments')) {
    function wpBugTracktorGetComments($post_id, $status="all") {
        
        wpBugTracktorCheckAdminPermissions();
        
        $output = '';
        
        if($status=='all') {
            $comments = get_comments(array(
                    'post_id' => $post_id
            ));
        } else {
            $comments = get_comments(array(
                    'post_id' => $post_id,
                    'status' => $status
            ));            
        }

        if ($comments) {
        
            $output .= '<table class="wpBugTracktorTable wpBugTracktorListCommentsTable">';
            foreach($comments as $comment) {
                if ($comment->comment_approved == 0 ) {
                    $comment_approval = '<a href="'.wp_nonce_url(admin_url('comment.php?c='.$comment->comment_ID.'&action=approvecomment'), 'approve-comment_'.$comment->comment_ID).'">'.__('Approve', 'wpbugtracktor').'</a>';
                } elseif($comment->comment_approved == 1) {
                    $comment_approval = '<a href="'.wp_nonce_url(admin_url('comment.php?c='.$comment->comment_ID.'&action=unapprovecomment'), 'approve-comment_'.$comment->comment_ID).'">'.__('Unapprove', 'wpbugtracktor').'</a>';
                } elseif($comment->comment_approved == 'spam') {
                    $comment_approval = __('Spam', 'wpbugtracktor') .'';
                } else {
                    $comment_approval = $comment->comment_approved;
                }
                
                $comment_approval .= ' | <a href="'.admin_url('comment.php?action=editcomment&c='.$comment->comment_ID).'">'.__('Edit', 'wpbugtracktor') .'</a>';
                $output .= '<tr class="wpBugTracktorListIssuesTablerow"><td class="wpBugTracktorListIssuesTablecell">'.get_avatar($comment, 32).'<br /><strong>'.$comment->comment_author.'</strong><br /><a href="mailto:'.$comment->comment_author_email.'">'.$comment->comment_author_email.'</a><br />'.$comment->comment_author_IP .'</td><td class="wpBugTracktorListIssuesTablecell">'.$comment->comment_content.'<br /><br />'.$comment_approval.'</td></tr>';
            }   
            $output .= '</table>';
            
        }
        
        return $output;
    }
}

if(!function_exists('wpBugTracktorViewPublicIssues')) {
    /**
     * 
     * 
     * @param integer $primkey
     * @param boolean $isadminpanel
     */
    function wpBugTracktorViewPublicIssues() {
        $wpBugTracktorOptions = get_option('wpBugTracktorAdminOptions');

        $primkey = intval($primkey);
        if($primkey==0) {
            $results = wpBugTracktorGetIssues();
        } else {
            $results = wpBugTracktorGetIssues('*', "WHERE `project_id`='{$primkey}' ORDER BY `project_id`, `status`, `type` ASC ;");
        }
        
        if(@isset($results[0]['primkey'])) {
            echo '<table class="wpBugTracktorTable wpBugTracktorListIssuesTable';
           
            echo '">';
            echo '<thead class="wpBugTracktorListIssuesTableHead"><tr class="wpBugTracktorListIssuesTablerow"><th class="wpBugTracktorListIssuesTableHeader">'.__('ID #', 'wpbugtracktor').'</th><th class="wpBugTracktorListIssuesTableHeader">'.__('Issue Title', 'wpbugtracktor').'</th><th class="wpBugTracktorListIssuesTableHeader">'.__('Type', 'wpbugtracktor').'</th><th class="wpBugTracktorListIssuesTableHeader">'.__('Status', 'wpbugtracktor').'</th><th class="wpBugTracktorListIssuesTableHeader">'.__('Severity', 'wpbugtracktor').'</th>';

            echo '</tr></thead><tbody class="wpBugTracktorListIssuesTableBody">';
            
            foreach($results as $result) {
                
                if (wpBugTracktorIsPublishedPage($result['primkey'])) {
                    $permalink = '<a href="'.get_permalink($result['post_id']).'">'.$result['title'].'</a>';
                } else {
                    $permalink = $result['title'];
                }
                
                switch (intval($result['type'])) {
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
                
                switch (intval($result['severity_priority'])) {
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
                
                echo '<tr class="wpBugTracktorListIssuesTablerow"><td class="wpBugTracktorListIssuesTablecell">'.$result['primkey'].'</td><td class="wpBugTracktorListIssuesTablecell">'.$permalink.'</td><td class="wpBugTracktorListIssuesTablecell">'.$type_name.'</td><td class="wpBugTracktorListIssuesTablecell">'.$result['status'].'</td><td class="wpBugTracktorListIssuesTablecell">'.$severity_name.'</td>';
                echo '</tr>';
            }
            
            echo '</tbody></table>';
            
        } else {
            _e('There are no issues to display.', 'wpbugtracktor');
        }       
    }
}


if(!function_exists('wpBugTracktorViewAdminIssueComments')) {
    /**
     * 
     * 
     * @param integer $primkey
     * @param boolean $isadminpanel
     */
    function wpBugTracktorViewAdminIssueComments($primkey=0, $isadminpanel=true) {
        
        wpBugTracktorCheckAdminPermissions();
        
        $wpBugTracktorOptions = get_option('wpBugTracktorAdminOptions');

        $primkey = intval($primkey);
        if($primkey==0) {
            $results = wpBugTracktorGetIssues('*', " ORDER BY `project_id`, `status`, `type` ASC ;");
        } else {
            $results = wpBugTracktorGetIssues('*', "WHERE `project_id`='{$primkey}' ORDER BY `project_id`, `status`, `type` ASC ;");
        }
        
        if(@isset($results[0]['primkey'])) {
            echo '<table class="wpBugTracktorTable wpBugTracktorListIssuesTable';
            
            if($isadminpanel) {
                echo ' widefat ';
            }
            
            echo '">';
            echo '<thead class="wpBugTracktorListIssuesTableHead"><tr class="wpBugTracktorListIssuesTablerow"><th class="wpBugTracktorListIssuesTableHeader">'.__('ID #', 'wpbugtracktor').'</th><th class="wpBugTracktorListIssuesTableHeader">'.__('Issue Title', 'wpbugtracktor').' <br /> '.__('Type', 'wpbugtracktor').' <br /> '.__('Status', 'wpbugtracktor').' <br /> '.__('Severity', 'wpbugtracktor').' </th>';
            if($isadminpanel) {
                echo '<th class="wpBugTracktorListIssuesTableHeader">'.__('Comments', 'wpbugtracktor').'</th>';
            }
            echo '</tr></thead><tbody class="wpBugTracktorListIssuesTableBody">';
            
            foreach($results as $result) {
                
                if (wpBugTracktorIsPublishedPage($result['primkey'])) {
                    $permalink = '<a href="'.get_permalink($result['post_id']).'">'.$result['title'].'</a>';
                } else {
                    $permalink = $result['title'];
                }
                
                switch (intval($result['type'])) {
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
                
                switch (intval($result['severity_priority'])) {
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
                
                echo '<tr class="wpBugTracktorListIssuesTablerow"><td class="wpBugTracktorListIssuesTablecell">'.$result['primkey'].'</td><td class="wpBugTracktorListIssuesTablecell">'.$permalink.'<br /> '.$type_name.' <br /> '.$result['status'].' <br /> '.$severity_name.'</td>';
                if($isadminpanel) {
                    echo '<td class="wpBugTracktorListIssuesTablecell">'.wpBugTracktorGetComments($result['post_id']).'</td>';
                }
                echo '</tr>';
            }
            
            echo '</tbody></table>';
            
        } else {
            _e('There are no issues to display.', 'wpbugtracktor');
        }       
    }
}


if(!function_exists('wpBugTracktorReportAnIssueStep1')) {
    /**
     * @todo Move this inline javascript into separate .js files
     */
    function wpBugTracktorReportAnIssueStep1() {
        $wpBugTracktorOptions = get_option('wpBugTracktorAdminOptions');  
        echo '
            <script type="text/javascript">
            <!--

                function wpBugTracktorReport(typeOfReport) {
                    jQuery("#wpBugTracktorReport").fadeIn();
                    jQuery("#wpBugTracktorReportAnIssueBug").fadeOut();
                    jQuery("#wpBugTracktorReportAnIssueFeatureRequest").fadeOut();          
                    
                    if(typeOfReport=="bug") {
                        jQuery("#wpBugTracktorReportAnIssueType").val(0);
                    }
                    
                    if(typeOfReport=="feature") {
                        jQuery("#wpBugTracktorReportAnIssueType").val(1);
                    }
                    
                    if(typeOfReport=="regression") {
                        jQuery("#wpBugTracktorReportAnIssueType").val(2);
                    }
                    
                    if(typeOfReport=="enhancement") {
                        jQuery("#wpBugTracktorReportAnIssueType").val(3);
                    }
                    
                    if(typeOfReport=="idea") {
                        jQuery("#wpBugTracktorReportAnIssueType").val(4);
                    }
                    
                    return false;
                }
                
                function wpBugTracktorGetComponents() {
                    var projectId = jQuery("#wpBugTracktorReportAnIssueProject").val();
                    
                    jQuery.post("'.admin_url( 'admin-ajax.php' ).'?action=wpbt_list_components", { projectid: projectId }, function(data) {
                        jQuery(\'#wpBugTracktorReportComponentList\').replaceWith(data);
                    }, \'html\');

                }
                
                function wpBugTracktorGetVersions() {
                    var projectId = jQuery("#wpBugTracktorReportAnIssueProject").val();
                    
                    jQuery.post("'.admin_url( 'admin-ajax.php' ).'?action=wpbt_list_versions", { projectid: projectId }, function(data) {
                        jQuery(\'#wpBugTracktorReportVersionList\').replaceWith(data);
                    }, \'html\');

                }
                
                function wpBugTracktorSubmitIssue() {
                
                    jQuery.post("'.admin_url( 'admin-ajax.php' ).'?action=wpbt_save_issue", {
                        wpBugTracktorReportIssueType: jQuery("#wpBugTracktorReportAnIssueType").val(),
                        wpBugTracktorReportIssueProject: jQuery("#wpBugTracktorReportAnIssueProject").val(),
                        wpBugTracktorReportComponentList: jQuery("#wpBugTracktorReportComponentList").val(),
                        wpBugTracktorReportVersionList: jQuery("#wpBugTracktorReportVersionList").val(),
                        wpBugTracktorReportFormTitle: jQuery("#wpBugTracktorReportFormTitle").val(),
                        wpBugTracktorReportFormDescription: jQuery("#wpBugTracktorReportFormDescription").val(),
                        wpBugTracktorReportSeverity: jQuery("#wpBugTracktorReportSeverity").val()
                    } , function(data) {
                        if(data != 0) {
                        ';
        
                        if($wpBugTracktorOptions['wpBugTracktorStatus']=='draft') {
                            echo 'alert("'.__('Your report has been submitted to our staff for approval.  Note that your submission will not show up on this website immediately.  First, a staff member must review it.  If they find your submission relevant, they may or may not choose to publish your submission.  There is no need to resubmit your report.  Thank you.', 'wpbugtracktor').'");
                                  window.location = " '.get_permalink($wpBugTracktorOptions['mainpage']).'"   ';
                        }
                        
                        if($wpBugTracktorOptions['wpBugTracktorStatus']=='publish') {
                            echo 'alert("'.__('Your report has been created.', 'wpbugtracktor').'");
                            window.location = data;    ';
                        }                        
        
                         echo ' 
                            
                        } else {
                            alert("'.__('Error! Your report could not be created.  Please double check that all required fields and security validations have been followed, then try again.', 'wpbugtracktor').'");
                        }
                    });


                    
                    
                    
                }
                
                jQuery(document).ready(function() {
                    wpBugTracktorGetVersions();
                    wpBugTracktorGetComponents();
                    ';
                    if(@$_GET['issue_tracker']=="bug") {
                        echo 'wpBugTracktorReport(\'bug\');';
                    }
                    if(@$_GET['issue_tracker']=="feature") {
                        echo 'wpBugTracktorReport(\'feature\');';
                    }     
                    if(@$_GET['issue_tracker']=="regression") {
                        echo 'wpBugTracktorReport(\'regression\');';
                    } 
                    if(@$_GET['issue_tracker']=="enhancement") {
                        echo 'wpBugTracktorReport(\'enhancement\');';
                    } 
                    if(@$_GET['issue_tracker']=="idea") {
                        echo 'wpBugTracktorReport(\'idea\');';
                    } 
                    if(@isset($_GET['wpbt_project']) && is_numeric($_GET['wpbt_project'])) {
                        echo 'jQuery("#wpBugTracktorReportAnIssueProject").val('.intval($_GET['wpbt_project']).');
                            wpBugTracktorGetComponents();wpBugTracktorGetVersions();';
                    }
                    echo '
                });

            //-->
            </script>
        ';
	    if (is_user_logged_in()) {
			echo '<p class="wpBugTracktorReportAnIssueStep1">';
			_e('Report a new issue:', 'wpbugtracktor');
			echo '</p>';
			echo '<button class="wpBugTracktorReportAnIssueButtons" id="wpBugTracktorReportAnIssueBug" onclick="wpBugTracktorReport(\'bug\');"><img src="'.plugins_url().'/wpbugtracktor/images/sm-bug.png" alt="" id="wpBugTracktorReportAnIssueBugImage" /> '.__('Bug/Regression', 'wpbugtracktor').'</button>';
			echo '<button class="wpBugTracktorReportAnIssueButtons" id="wpBugTracktorReportAnIssueFeatureRequest" onclick="wpBugTracktorReport(\'feature\');"><img src="'.plugins_url().'/wpbugtracktor/images/lightbulb.png" alt="" id="wpBugTracktorReportAnIssueFeatureRequestImage" /> '.__('Feature Request, Enhancement, or Idea', 'wpbugtracktor').'</button>';
			echo '<div id="wpBugTracktorReport" style="display:none;">
					<form id="wpBugTracktorReportForm" method="post" action="">
						<label class="wpBugTracktorFormLabel">'.__('Issue Type', 'wpbugtracktor').':
							<select class="wpBugTracktorFormSelect" name="wpBugTracktorReportAnIssueType" id="wpBugTracktorReportAnIssueType">
								<option value="0">'.__('Bug', 'wpbugtracktor').'</option>
								<option value="1">'.__('Feature Request', 'wpbugtracktor').'</option>
								<option value="2">'.__('Regression', 'wpbugtracktor').'</option>
								<option value="3">'.__('Enhancement', 'wpbugtracktor').'</option>
								<option value="4">'.__('Idea', 'wpbugtracktor').'</option>
							</select>
						</label>
						<label class="wpBugTracktorFormLabel">'.__('Project', 'wpbugtracktor').':
							<select class="wpBugTracktorFormSelect" name="wpBugTracktorReportAnIssueProject" id="wpBugTracktorReportAnIssueProject" onchange="wpBugTracktorGetComponents();wpBugTracktorGetVersions();">';
							
							$wpBugTracktorGetProjects = wpBugTracktorGetProjects('`primkey`, `title`');
							if(isset($wpBugTracktorGetProjects[0]['primkey'])) {
								foreach($wpBugTracktorGetProjects as $wpBugTracktorGetProject) {
									echo '<option value="'.$wpBugTracktorGetProject['primkey'].'">'.$wpBugTracktorGetProject['title'].'</option>';
								}
							}
							
							echo '
							</select>
						</label>  
						<label class="wpBugTracktorFormLabel" id="wpBugTracktorReportVersion">'.__('Version', 'wpbugtracktor').':
							<select class="wpBugTracktorFormSelect" name="wpBugTracktorReportVersionList" id="wpBugTracktorReportVersionList">
							</select>
						</label>                    
						<label class="wpBugTracktorFormLabel" id="wpBugTracktorReportComponent">'.__('Component', 'wpbugtracktor').':
							<select class="wpBugTracktorFormSelect" name="wpBugTracktorReportComponentList" id="wpBugTracktorReportComponentList">
							</select>
						</label>     
						<label class="wpBugTracktorFormLabel">'.__('Severity', 'wpbugtracktor').':
							<select class="wpBugTracktorFormSelect" name="wpBugTracktorReportSeverity" id="wpBugTracktorReportSeverity">
								<option value="0">'.__('Non-critical', 'wpbugtracktor').'</option>
								<option value="1">'.__('Low priority', 'wpbugtracktor').'</option>
								<option value="2">'.__('Medium', 'wpbugtracktor').'</option>
								<option value="3">'.__('Above Average', 'wpbugtracktor').'</option>
								<option value="4">'.__('Critical', 'wpbugtracktor').'</option>
								<option value="5">'.__('Emergency', 'wpbugtracktor').'</option>
							</select>
						</label>                    
						<label class="wpBugTracktorFormLabel">'.__('Issue Title', 'wpbugtracktor').': <input type="text" name="wpBugTracktorReportFormTitle" id="wpBugTracktorReportFormTitle" class="wpBugTracktorFormFullWidthText" /> </label>
						<label class="wpBugTracktorFormLabel">'.__('Issue Description', 'wpbugtracktor').': <textarea name="wpBugTracktorReportFormDescription" id="wpBugTracktorReportFormDescription" class="wpBugTracktorFormTextarea"></textarea> </label>';
						
							echo ' 
							<label class="wpBugTracktorFormLabel"><button onclick="wpBugTracktorSubmitIssue(); return false;">'.__('Submit', 'wpbugtracktor').'</button> </label>
					</form>
				  </div>
			';
		}
		else {
			_e('<a href="http://copycat.fr/amember/signup">Register</a> or <a href="http://copycat.fr/amember/login">Login</a> to report a new issue.', 'wpbugtracktor');
		}
    }
}

if(!function_exists('wpBugTracktorReportAnIssueStep2')) {
    function wpBugTracktorReportAnIssueStep2($primkey) {
        global $wpdb;
        
        $issues = wpBugTracktorGetIssues('*', "WHERE `primkey`='{$primkey}'");
        if(@isset($issues[0]['primkey'])) {
            /*
            $issues[0]['primkey'];
            $issues[0]['title'];
            $issues[0]['description'];
            $issues[0]['type'];
            $issues[0]['status'];
            $issues[0]['reporter_id'];
            $issues[0]['reporter_email'];
            $issues[0]['reporter_ip'];
            $issues[0]['owner_id'];
            $issues[0]['project_id'];
            $issues[0]['project_component_id'];
            $issues[0]['severity_priority'];
            $issues[0]['version_reported'];
            $issues[0]['target_fix_for_milestone_id'];
            $issues[0]['tags'];
            $issues[0]['date_reported'];
            $issues[0]['date_last_modified'];
            $issues[0]['post_id'];
            $issues[0]['related_issue_ids'];
            $issues[0]['email_updates_to_these_users'];
            $issues[0]['attachments'];
             * 
             */
            
            if($issues[0]['project_id']!=0) {
                $project_results = $wpdb->get_results("SELECT * FROM `{$wpdb->prefix}wpbugtracktor_projects` WHERE `primkey`='".intval($issues[0]['project_id'])."' ;", ARRAY_A);
                if(@isset($project_results[0]['primkey'])) {
                    $project_primkey = $project_results[0]['primkey'];
                    $project_title = $project_results[0]['title'];
                    $project_description = $project_results[0]['description'];
                    $project_prefix = $project_results[0]['project_prefix'];
                    $project_owner_id = $project_results[0]['owner_id'];
                    $project_users_assigned = $project_results[0]['users_assigned'];
                } else {
                    $project_primkey = 0;
                    $project_title = __('Untitled', 'wpbugtracktor');
                    $project_description = __('No description', 'wpbugtracktor');
                    $project_prefix = '';
                    $project_owner_id = 0;
                    $project_users_assigned = 0;                    
                }
            } else {
                $project_primkey = 0;
                $project_title = __('Untitled', 'wpbugtracktor');
                $project_description = __('No description', 'wpbugtracktor');
                $project_prefix = '';
                $project_owner_id = 0;
                $project_users_assigned = 0;                    
            }
            
            $issue_type = '';
            switch ($issues[0]['type']) {
                case 0:
                    $issue_type = __('Bug', 'wpbugtracktor');
                break;
                case 1:
                    $issue_type = __('Feature Request', 'wpbugtracktor');
                break;
                case 2:
                    $issue_type = __('Regression', 'wpbugtracktor');
                break;
                case 3:
                    $issue_type = __('Enhancement', 'wpbugtracktor');
                break;
                case 4:
                    $issue_type = __('Idea', 'wpbugtracktor');
                break;            
            }            
            
            $version_name = '';
            if($issues[0]['version_reported']!=0) {
                $version_results = $wpdb->get_results("SELECT * FROM `{$wpdb->prefix}wpbugtracktor_milestones` WHERE `primkey`='".intval($issues[0]['version_reported'])."' ;", ARRAY_A);
                if(@isset($version_results[0]['primkey'])) {
                    $version_name = $version_results[0]['title'];
                    $version_number = $version_results[0]['version_number'];
                    $version_description = $version_results[0]['description'];
                }                
                
            } 
            
            $component_name = '';
            if($issues[0]['project_component_id']!=0) {
                $component_results = $wpdb->get_results("SELECT * FROM `{$wpdb->prefix}wpbugtracktor_components` WHERE `primkey`='".intval($issues[0]['project_component_id'])."' ;", ARRAY_A);
                if(@isset($component_results[0]['primkey'])) {
                    $component_name = $component_results[0]['title'];
                    $component_description = $component_results[0]['description'];
                }                
                
            }             
            
            echo '
            <table class="wpBugTracktorReport">
                <tr>
                    <td>'.__('Title', 'wpbugtracktor').'</td>
                    <td><h2 class="wpBugTracktorReportTitle">'.$project_prefix.$issues[0]['primkey'].' - '.$issues[0]['title'].' </h2></td>
                </tr>
                <tr>
                    <td>'.__('Type', 'wpbugtracktor').'</td>
                    <td>
                        <div class="wpBugTracktorReportType">
                        '.$issue_type.'
                        </div>
                    </td>
                </tr>    
                <tr>
                    <td>'.__('Status', 'wpbugtracktor').'</td>
                    <td>
                        <div class="wpBugTracktorReportStatus">
                        '.$issues[0]['status'].'
                        </div>
                    </td>
                </tr>                
                <tr>
                    <td>'.__('Project', 'wpbugtracktor').'</td>
                    <td>
                        <div class="wpBugTracktorReportProject">
                            <span  class="wpBugTracktorReportProjectTitle"><strong>'.$project_title.'</strong></span><br />
                            <span class="wpBugTracktorReportProjectDescription">'.$project_description.'</span>
                        </div>
                    </td>
                </tr>   
 ';

            if($version_name!= '') {
                echo '
                <tr>
                    <td>'.__('Version', 'wpbugtracktor').'</td>
                    <td>
                        <div class="wpBugTracktorReportComponent">
                            <span class="wpBugTracktorReportComponentTitle"><strong>'.$version_number.' ('.$version_name.')</strong></span><br />
                            <span class="wpBugTracktorReportComponentDescription">'.$version_description.'</span>                        
                        
                        </div>
                    </td>
                </tr> ';
            }            
            
            if($component_name!= '') {
                echo '
                <tr>
                    <td>'.__('Component', 'wpbugtracktor').'</td>
                    <td>
                        <div class="wpBugTracktorReportComponent">
                            <span class="wpBugTracktorReportComponentTitle"><strong>'.$component_name.'</strong></span><br />
                            <span class="wpBugTracktorReportComponentDescription">'.$component_description.'</span>                        
                        
                        </div>
                    </td>
                </tr> ';
            }
            echo '</table>';
            
            echo '<h3>'.__('Description', 'wpbugtracktor').'</h3>';
            
            if($issues[0]['reporter_id']!=0) {
                $user_info = get_userdata($issues[0]['reporter_id']);
                $username = $user_info->user_login;
            } else {
                $username = __('Guest', 'wpbugtracktor');
            }
            
            
            $submittedDate = date("F d, Y", strtotime($issues[0]['date_reported']));
                    
            echo ' 
             <table class="wpBugTracktorReport">   
                <tr>
                    <td>
                        <div class="wpBugTracktorReportDescription">
                        '.$issues[0]['description'].'<br /><br /><br />
                        </div>
                    </td>
                </tr>
                <tr>
                    <td>
                        <div class="wpBugTracktorReportFooter">
                            '.__('Submitted by', 'wpbugtracktor').' '.$username.' '.__('on', 'wpbugtracktor').' '.$submittedDate.' 
                        </div>
                    </td>
                </tr>

            </table>
            ';
            
        }
    }
}

?>