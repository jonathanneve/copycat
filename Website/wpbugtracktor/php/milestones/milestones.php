<?php

if(!function_exists('wpBugTracktorCreateMilestone')) {
    function wpBugTracktorCreateMilestone($title, $version_number, $description, $project_id, $is_released, $start_date, $release_date) {
        global $wpdb;
        $wpdb->query("INSERT INTO `{$wpdb->table_prefix}wpbugtracktor_milestones` (`primkey`, `title`, `version_number`, `description`, `project_id`, `is_released`, `start_date`, `release_date`) VALUES (NULL, '{$title}', '{$version_number}', '{$description}', '{$project_id}', '{$is_released}', '{$start_date}', '{$release_date}');");
    }
}

?>