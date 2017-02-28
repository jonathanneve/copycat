<?php

/**
 *
 * Checks to see if a column exists in a mysql table, if not, it creates it
 *
 * @global object $wpdb
 * @param string $db
 * @param string $column
 * @param string $column_attr
 */
function wpscbtAddColumnIfNotExist($db, $column, $column_attr = "VARCHAR( 255 ) NULL" ){
        global $wpdb;
        $exists = false;
        $columns = $wpdb->get_results( "show columns from $db" , ARRAY_A );
        foreach($columns as $c) {
                if($c['Field'] == $column){
                        $exists = true;
                        break;
                }
        }      
        if(!$exists){
                $wpdb->query("ALTER TABLE `$db` ADD `$column`  $column_attr");
        }
}

/**
*
* This method creates the database schema during installation
*
* @global object $wpdb
* @global int $wpstorecart_version_int
*/
if(!function_exists('wpscbtInstallWpms')) {
	function wpscbtInstallWpms() {
	
		global $wpdb;
		
                // ===================================================================
		$table_name = $wpdb->prefix . "wpbugtracktor_projects";
		if($wpdb->get_var("show tables like '$table_name'") != $table_name) {
	
			$sql = "
                            CREATE TABLE `{$table_name}` (
                            `primkey` INT NOT NULL AUTO_INCREMENT PRIMARY KEY ,
                            `title` VARCHAR( 768 ) NOT NULL ,
                            `description` TEXT NOT NULL ,
                            `project_prefix` VARCHAR( 32 ) NOT NULL ,
                            `owner_id` INT NOT NULL ,
                            `users_assigned` TEXT NOT NULL 
                            );                            
                            ";
	
	
			require_once(ABSPATH . 'wp-admin/includes/upgrade.php');
			dbDelta($sql);
		}
                
                
                // ===================================================================
		$table_name = $wpdb->prefix . "wpbugtracktor_components";
		if($wpdb->get_var("show tables like '$table_name'") != $table_name) {
	
			$sql = "
                            CREATE TABLE `{$table_name}` (
                            `primkey` INT NOT NULL AUTO_INCREMENT PRIMARY KEY ,
                            `title` VARCHAR( 768 ) NOT NULL ,
                            `description` TEXT NOT NULL ,
                            `project_id` INT NOT NULL
                            );                           
	                    ";
	
	
			require_once(ABSPATH . 'wp-admin/includes/upgrade.php');
			dbDelta($sql);
		}                

                
                
                // ===================================================================
		$table_name = $wpdb->prefix . "wpbugtracktor_milestones";
		if($wpdb->get_var("show tables like '$table_name'") != $table_name) {
	
			$sql = "
                            CREATE TABLE `{$table_name}` (
                            `primkey` INT NOT NULL AUTO_INCREMENT PRIMARY KEY ,
                            `title` VARCHAR( 768 ) NOT NULL ,
                            `version_number` VARCHAR( 256 ) NOT NULL ,
                            `description` TEXT NOT NULL ,
                            `project_id` INT NOT NULL ,
                            `is_released` BOOLEAN NOT NULL ,
                            `start_date` INT( 8 ) NOT NULL ,
                            `release_date` INT( 8 ) NOT NULL
                            );                           
	                    ";
	
	
			require_once(ABSPATH . 'wp-admin/includes/upgrade.php');
			dbDelta($sql);
		}                  


                // ===================================================================
		$table_name = $wpdb->prefix . "wpbugtracktor_issues";
		if($wpdb->get_var("show tables like '$table_name'") != $table_name) {
	
			$sql = "
                            CREATE TABLE `{$table_name}` (`primkey` INT NOT NULL AUTO_INCREMENT PRIMARY KEY, `title` VARCHAR(768) NOT NULL, `description` TEXT NOT NULL, `type` INT NOT NULL, `status` VARCHAR( 256 ) NOT NULL, `reporter_id` INT NOT NULL, `reporter_email` VARCHAR( 512 ) NOT NULL, `reporter_ip` VARCHAR(45) NOT NULL, `owner_id` INT NOT NULL, `project_id` INT NOT NULL, `project_component_id` INT NOT NULL, `severity_priority` INT NOT NULL, `version_reported` INT NOT NULL, `target_fix_for_milestone_id` INT NOT NULL, `tags` TEXT NOT NULL, `date_reported` INT(8) NOT NULL, `date_last_modified` INT(8) NOT NULL, `post_id` INT NOT NULL, `related_issue_ids` TEXT NOT NULL, `email_updates_to_these_users` TEXT NOT NULL, `attachments` TEXT NOT NULL);                           
	                    ";
	
	
			require_once(ABSPATH . 'wp-admin/includes/upgrade.php');
			dbDelta($sql);
		}                   
           
                
                
                   
                
                
        }
}


/**
 *
 * Installs wpBugTractor
 *
 * @global object $wpdb
 * @global int $wpstorecart_version_int
 * @return type
 */
if(!function_exists('wpscbtInstall')) {
	function wpscbtInstall() {
		global $wpdb;
	
		if (function_exists('is_multisite') && is_multisite()) {
			// check if it is a network activation - if so, run the activation function for each blog id
			if (isset($_GET['networkwide']) && ($_GET['networkwide'] == 1)) {
				$old_blog = $wpdb->blogid;
				// Get all blog ids
				$blogids = $wpdb->get_col($wpdb->prepare("SELECT blog_id FROM $wpdb->blogs"));
				foreach ($blogids as $blog_id) {
					switch_to_blog($blog_id);
					wpscbtInstallWpms();
				}
				switch_to_blog($old_blog);
				return;
			}
		}
		wpscbtInstallWpms();

	}
}


?>