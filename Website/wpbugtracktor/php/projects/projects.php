<?php

if(!function_exists('wpBugTracktorCreateProject')) {
    function wpBugTracktorCreateProject($title, $description, $project_prefix, $owner_id, $users_assigned, $current_released_milestones, $archive_milestones) {
        global $wpdb;
        $wpdb->query("INSERT INTO `{$wpdb->prefix}wpbugtracktor_projects` (`primkey`, `title`, `description`, `project_prefix`, `owner_id`, `users_assigned`, `current_released_milestones`, `archive_milestones`) VALUES (NULL, '{$title}', '{$description}', '{$project_prefix}', '{$owner_id}', '{$users_assigned}', '{$current_released_milestones}', '{$archive_milestones}');");
    }
}

if(!function_exists('wpBugTracktorGetProjects')) {
    function wpBugTracktorGetProjects($selection='*', $whereclause=NULL) {
        global $wpdb;
        return $wpdb->get_results("SELECT {$selection} FROM `{$wpdb->prefix}wpbugtracktor_projects` {$whereclause};", ARRAY_A);
    }
}

if(!function_exists('wpBugTracktorShowPublicProjects')) {
    function wpBugTracktorShowPublicProjects($primkey=0) {
        
        $wpBugTracktorOptions = get_option('wpBugTracktorAdminOptions');
        
        $primkey = intval($primkey);
        if($primkey==0) {
            $results = wpBugTracktorGetProjects();
        } else {
            $results = wpBugTracktorGetProjects('*', "WHERE `primkey`='{$primkey}';");
        }
        
        if(@isset($results[0]['primkey'])) {
            echo '<table class="wpBugTracktorTable wpBugTracktorProjectTable">';
            echo '<thead class="wpBugTracktorProjectTableHead"><tr class="wpBugTracktorProjectTablerow"><th class="wpBugTracktorProjectTableHeader">'.__('ID #', 'wpbugtracktor').'</th><th class="wpBugTracktorProjectTableHeader">'.__('Project Name', 'wpbugtracktor').'</th><th class="wpBugTracktorProjectTableHeader">'.__('Description', 'wpbugtracktor').'</th><th class="wpBugTracktorProjectTableHeader">'.__('Prefix/Abbreviation', 'wpbugtracktor').'</th><th class="wpBugTracktorProjectTableHeader">'.__('Project Lead', 'wpbugtracktor').'</th></tr></thead><tbody class="wpBugTracktorProjectTableBody">';
            foreach($results as $result) {
                    $display_name = '';
                    if(intval($result['owner_id'])==0) {
                        $display_name  = __('Unassigned', 'wpbugtracktor');
                    } else {
                        global $user_info;
                        $user_info = get_userdata(intval($result['owner_id']));
                        $display_name = $user_info->display_name;
                    }
                    if(strpos(get_permalink($wpBugTracktorOptions['mainpage']),'?')===false) {
                        $permalink = get_permalink($wpBugTracktorOptions['mainpage']) .'?list_issues_from_project='.$result['primkey'];
                    } else {
                        $permalink = get_permalink($wpBugTracktorOptions['mainpage']) .'&list_issues_from_project='.$result['primkey'];
                    }                    
                echo '<tr class="wpBugTracktorProjectTablerow"><td class="wpBugTracktorProjectTablecell">'.$result['primkey'].'</td><td class="wpBugTracktorProjectTablecell"><a href="'.$permalink.'">'.$result['title'].'</a></td><td class="wpBugTracktorProjectTablecell">'.$result['description'].'</td><td class="wpBugTracktorProjectTablecell">'.$result['project_prefix'].'</td><td class="wpBugTracktorProjectTablecell">'.$display_name.'</td></tr>';
            }
            echo '</tbody></table>';
        }
        
    }
}


?>