<?php

wpbugtracktor_admin();

if (!function_exists('wpBugTracktorAssignCapabilities')) {
    function wpBugTracktorAssignCapabilities($blog_id=NULL) {
        require_once (ABSPATH.'/wp-includes/capabilities.php');
        global $wp_roles;
        if($blog_id!=NULL) {
            switch_to_blog($blog_id);
        }
        if(is_object($wp_roles)) {
            $wp_roles->add_cap('administrator', 'manage_wpbugtracktor'); // All administrators can manage wpbugtracktor
            // Creates the new wpbugtracktor_manager role, which allows other users to manage wpbugtracktor
            add_role('wpbugtracktor_manager', 'wpBugTracktor Manager', array('manage_wpbugtracktor', 'read', 'upload_files', 'publish_posts', 'edit_published_posts', 'publish_pages', 'edit_published_pages'));
            $wp_roles->add_cap('wpbugtracktor_manager', 'read');
            $wp_roles->add_cap('wpbugtracktor_manager', 'upload_files');
            $wp_roles->add_cap('wpbugtracktor_manager', 'publish_pages');
            $wp_roles->add_cap('wpbugtracktor_manager', 'publish_posts');
            $wp_roles->add_cap('wpbugtracktor_manager', 'edit_published_posts');
            $wp_roles->add_cap('wpbugtracktor_manager', 'edit_published_pages');
            $wp_roles->add_cap('wpbugtracktor_manager', 'manage_wpbugtracktor');
        }
        if($blog_id!=NULL) {
            restore_current_blog();
        }
    }

}


if (!function_exists('wpBugTracktorCheckAdminPermissions')) {
    /**
     * Checks for appropriate permissions and kills Wordpress if they're not found.
     */
    function wpBugTracktorCheckAdminPermissions() {
        if (function_exists('current_user_can') && !current_user_can('manage_wpbugtracktor')) {
            wp_die(__('wpBugTracktor: You do not have sufficient permissions to access this page.', 'wpbugtracktor'));
        }
    }

}

if (!function_exists('wpBugTracktorAdminHead')) {
    /**
        * Loads the appropriate CSS and Javascript for the admin panel
        */
    function wpBugTracktorAdminHead() {
        wp_register_style('wpsc-table-jui', plugins_url() . '/wpbugtracktor/css/demo_table_jui.css');
        wp_enqueue_style('wpsc-table-jui');    
        wp_enqueue_script('jquery');
        wp_enqueue_script('jquery-ui-core');
        wp_enqueue_script('jquery-ui-accordion');
        wp_enqueue_script('jquery-ui-tabs');
        wp_enqueue_script('jquery-ui-sortable');
        wp_enqueue_script('jquery-ui-draggable');
        wp_enqueue_script('jquery-ui-droppable');
        wp_enqueue_script('jquery-ui-selectable');
        wp_enqueue_script('jquery-ui-datepicker');
        wp_enqueue_script('jquery-ui-resizable');
        wp_enqueue_script('jquery-ui-dialog');
        wp_enqueue_script('wpsc-jquery-datatables', plugins_url() . '/wpbugtracktor/js/jquery.dataTables.min.js');
        wp_enqueue_script('wpsc-jeditable', plugins_url() . '/wpbugtracktor/js/jquery.jeditable.mini.js');
        wp_enqueue_script('wpsc-jquerytools-dateinput', plugins_url() . '/wpbugtracktor/js/jquery.tools.dateinput.min.js');
        wp_enqueue_script('wpscstraphael', plugins_url().'/wpbugtracktor/js/tufte-graph/raphael.js', array('jquery'), '1.3.2');
        wp_enqueue_script('wpscstenumerable', plugins_url().'/wpbugtracktor/js/tufte-graph/jquery.enumerable.js', array('jquery'), '1.3.2');
        wp_enqueue_script('wpscsttufte', plugins_url().'/wpbugtracktor/js/tufte-graph/jquery.tufte-graph.js', array('jquery'), '1.3.2');
        wp_enqueue_style('tufte-admin-ui-css', plugins_url().'/wpbugtracktor/js/tufte-graph/tufte-graph.css', false, 2, false);        
    }
}


if (!function_exists('wpBugTracktorAdminPages')) {
    /**
        * The admin pages
        */
    function wpBugTracktorAdminPages() {
        $mainPage = add_menu_page('wpBugTracktor - '.__('Open Source Issue Tracker','wpbugtracktor'), 'wpBugTracktor', 'manage_wpbugtracktor', 'wpbugtracktor-new-admin', 'wpBugTracktorAdminPageMain', plugins_url() . '/wpbugtracktor/images/bug.png');
        $settingsPage = add_submenu_page('wpbugtracktor-new-admin', __('Configure Settings','wpbugtracktor').' - wpBugTracktor ', __('Configure Settings','wpbugtracktor'), 'manage_wpbugtracktor', 'wpbugtracktor-new-settings', 'wpBugTracktorAdminPageSettings');
        $addNewProjectPage = add_submenu_page('wpbugtracktor-new-admin', __('Add New Project','wpbugtracktor').' - wpBugTracktor ', __('Add New Project','wpbugtracktor'), 'manage_wpbugtracktor', 'wpbugtracktor-new-project', 'wpBugTracktorAdminPageAddNewProject');
        $editProjectPage = add_submenu_page('wpbugtracktor-new-admin', __('Edit Projects','wpbugtracktor').' - wpBugTracktor ', __('Edit Projects','wpbugtracktor'), 'manage_wpbugtracktor', 'wpbugtracktor-edit-projects', 'wpBugTracktorAdminPageEditProjects');
        $versionsPage = add_submenu_page('wpbugtracktor-new-admin', __('Manage Versions','wpbugtracktor').' - wpBugTracktor ', __('Manage Versions','wpbugtracktor'), 'manage_wpbugtracktor', 'wpbugtracktor-edit-versions', 'wpBugTracktorAdminPageVersions');
        $issuesPage = add_submenu_page('wpbugtracktor-new-admin', __('Manage Issues','wpbugtracktor').' - wpBugTracktor ', __('Manage Issues','wpbugtracktor'), 'manage_wpbugtracktor', 'wpbugtracktor-issues', 'wpBugTracktorAdminPageIssues');
        $manageIssuesPage = add_submenu_page('wpbugtracktor-new-admin', __('Manage Comments','wpbugtracktor').' - wpBugTracktor ', __('Manage Comments','wpbugtracktor'), 'manage_wpbugtracktor', 'wpbugtracktor-manage-comments', 'wpBugTracktorAdminPageManageComments');
        add_action('admin_head-' . $mainPage, 'wpBugTracktorAdminHead');
        add_action('admin_head-' . $settingsPage, 'wpBugTracktorAdminHead');
        add_action('admin_head-' . $addNewProjectPage, 'wpBugTracktorAdminHead');
        add_action('admin_head-' . $editProjectPage, 'wpBugTracktorAdminHead');
        add_action('admin_head-' . $versionsPage, 'wpBugTracktorAdminHead');
        add_action('admin_head-' . $issuesPage, 'wpBugTracktorAdminHead');
        add_action('admin_head-' . $manageIssuesPage, 'wpBugTracktorAdminHead');
    }

}

//wpBugTracktorAssignCapabilities();
add_action('wpbugtracktor_admin', 'wpBugTracktorAssignCapabilities', 1); // Applies our capabilities function to our wpsc_admin action hook    
add_action('admin_menu', 'wpBugTracktorAdminPages', 1); // Applies our admin pages to admin_menu hook, so that we can see admin pages.
add_action('wpmu_new_blog', 'wpBugTracktorAssignCapabilities'); // When a new WPMU blog is created, this assures that the new blog also has the admin capabilities

?>