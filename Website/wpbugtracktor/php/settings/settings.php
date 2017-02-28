<?php

if(!function_exists('wpBugTracktorReturnValidManagers')) {
    /**
     * Returns an array of IDs of users who can manage issues
     * 
     * @global object $wpdb
     * @return array 
     */
    function wpBugTracktorReturnValidManagers() {
        global $wpdb;
        
        $valid_managers = array();

        $search = $wpdb->get_results("SELECT `ID` FROM `{$wpdb->prefix}users` ORDER BY `ID`;", ARRAY_A);

        foreach ($search as $userid) {
            if (user_can($userid['ID'],  'manage_wpbugtracktor')) {
                $valid_managers[] = $userid['ID'];
            }
        } 

        return $valid_managers;
    }
}


if(!function_exists('wpbtGdCheck')) {
        /**
         * Tries to determine if GD is installed
         * 
         * @return boolean 
         */
        function wpbtGdCheck() {
            if (@function_exists('imagecreatetruecolor')) {
                return true;
            }
            elseif (@function_exists('imagecreate')) {
                return true;
            }
            else {
                return false;
            }
        }      
}

if(!class_exists('wpBugTracktorSettings')) {
        /**
         * wpBugTracktorSettings class
         */
	class wpBugTracktorSettings {
		/**  
		 * @var string $adminOptionsName Just the name of the wpBugTracktor options in the Wordpress database 
		 */
		var $adminOptionsName = "wpBugTracktorAdminOptions";
		
		/**
		 * @var array $wpBugTracktorSettings The actual wpBugTracktor options.
		 */
		var $wpBugTracktorSettings = null;	
		
		/**
		 * Constructor method, returns options
		 */
		function __construct() {
			$this->wpBugTracktorSettings = $this->getAdminOptions('flush');
		}
		
		/**
		*
		* Returns an array of admin options
		*
		* @param string $action
		* @return array
		*/
		function getAdminOptions($action=NULL) {
		
			$apAdminOptions = array(    
                            'mainpage' => '', // The page where the wpBugTracktor shortcode is placed
                            'admin_capability' => 'administrator', // Minimum role to manage wpBugTracktor
                            'permission_to_report_issues' => 'registered_users', // Possible Values: guests, registered_users, staff_only
                            'permission_to_comment' => 'registered_users', // Possible Values: guests, registered_users, staff_only
                            'toggle_issue_primkey' => 'false',
                            'toggle_issue_title' => 'false',
                            'toggle_issue_description' => 'false',
                            'toggle_issue_type' => 'false',
                            'toggle_issue_status' => 'false',
                            'toggle_issue_reporter_id' => 'false',
                            'toggle_issue_reporter_email' => 'false',
                            'toggle_issue_reporter_ip' => 'false',
                            'toggle_issue_owner_id' => 'false',
                            'toggle_issue_project_id' => 'false',
                            'toggle_issue_project_component_id' => 'false',
                            'toggle_issue_severity_priority' => 'false',
                            'toggle_issue_version_reported' => 'false',
                            'toggle_issue_target_fix_for_milestone_id' => 'false',
                            'toggle_issue_tags' => 'false',
                            'toggle_issue_date_reported' => 'false',
                            'toggle_issue_date_last_modified' => 'false',
                            'toggle_issue_post_id' => 'false',
                            'toggle_issue_related_issue_ids' => 'true',
                            'toggle_issue_email_updates_to_these_users' => 'true',
                            'toggle_issue_attachments' => 'true',
                            'wpBugTracktorStatus' => 'draft'
			);
		
		
			if($this->wpBugTracktorSettings!=NULL) {
				if($action!='flush') {
					$wpBugTracktorOptions = $this->wpBugTracktorSettings;
				} else {
					$this->wpBugTracktorSettings = NULL;
					$wpBugTracktorOptions = get_option($this->adminOptionsName);
				}
			} else {
				$wpBugTracktorOptions = get_option($this->adminOptionsName);
			}
		
			if (!empty($wpBugTracktorOptions)) {
				foreach ($wpBugTracktorOptions as $key => $option) {
					$apAdminOptions[$key] = $option;
				}
			}

			update_option($this->adminOptionsName, $apAdminOptions);
		
			return $apAdminOptions;
		}
                
                
                function setAdminOptions() {
                    global $wpdb;
                            $wpBugTracktorOptions = get_option($this->adminOptionsName);
                            if (isset($_POST['mainpage'])) {
                                    $wpBugTracktorOptions['mainpage'] = $wpdb->escape($_POST['mainpage']);
                            }
                            if (isset($_POST['permission_to_report_issues'])) {
                                    $wpBugTracktorOptions['permission_to_report_issues'] = $wpdb->escape($_POST['permission_to_report_issues']);
                            } 	
                            if (isset($_POST['permission_to_comment'])) {
                                    $wpBugTracktorOptions['permission_to_comment'] = $wpdb->escape($_POST['permission_to_comment']);
                            } 	                            

                            if (isset($_POST['admin_capability'])) {
                                    global $wp_roles;
                                    $wpBugTracktorOptions['admin_capability'] = $wpdb->escape($_POST['admin_capability']);
                                    if($wpBugTracktorOptions['admin_capability']=='administrator') {
                                        $wp_roles->remove_cap( 'editor', 'manage_wpbugtracktor' );
                                        $wp_roles->remove_cap( 'author', 'manage_wpbugtracktor' );
                                        $wp_roles->remove_cap( 'contributor', 'manage_wpbugtracktor' );
                                    }
                                    if($wpBugTracktorOptions['admin_capability']=='editor') {
                                        $wp_roles->add_cap( 'administrator', 'manage_wpbugtracktor' );
                                        $wp_roles->add_cap( 'editor', 'manage_wpbugtracktor' );
                                        $wp_roles->remove_cap( 'author', 'manage_wpbugtracktor' );
                                        $wp_roles->remove_cap( 'contributor', 'manage_wpbugtracktor' );
                                    }
                                    if($wpBugTracktorOptions['admin_capability']=='author') {
                                        $wp_roles->add_cap( 'administrator', 'manage_wpbugtracktor' );
                                        $wp_roles->add_cap( 'editor', 'manage_wpbugtracktor' );
                                        $wp_roles->add_cap( 'author', 'manage_wpbugtracktor' );
                                        $wp_roles->remove_cap( 'contributor', 'manage_wpbugtracktor' );
                                    }
                                    if($wpBugTracktorOptions['admin_capability']=='contributor') {
                                        $wp_roles->add_cap( 'administrator', 'manage_wpbugtracktor' );
                                        $wp_roles->add_cap( 'editor', 'manage_wpbugtracktor' );
                                        $wp_roles->add_cap( 'author', 'manage_wpbugtracktor' );
                                        $wp_roles->add_cap( 'contributor', 'manage_wpbugtracktor' );
                                    }
                            }
                            
                            if(@isset($_POST['wpBugTracktorStatus'])) { // Update options
                                $wpBugTracktorOptions['wpBugTracktorStatus'] = $wpdb->escape($_POST['wpBugTracktorStatus']);
                            }                            

                            update_option($this->adminOptionsName, $wpBugTracktorOptions);

                 
                
                
                
                }
		
		
	}
}

?>