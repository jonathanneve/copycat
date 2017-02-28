<?php
if(!function_exists('wpBugTracktorMainShortcode')) {
    /**
     *
     * Handles the wpBugTracktor shortcode
     * 
     * @param array $atts
     * @param string $content
     * @return string 
     */
    function wpBugTracktorMainShortcode($atts, $content = null) {
        global $wpdb, $current_user;
        
        
        $wpBugTracktorOptions = get_option('wpBugTracktorAdminOptions');        
        
        extract(shortcode_atts(array(
                'display' => 'mainpage',
                'primkey' => '0',
                'quantity' => 'unset',
                'usetext' => 'true',
                'usepictures' => 'false',
                'thecategory' => '',
                'displaytype' => '',
                'orderby' => '',
                'ordertype' => '',
                'allowguests' => 'false',
        ), $atts));  
        
        wp_register_style( 'wpbugtracktor', plugins_url().'/wpbugtracktor/css/wpbugtracktor.css' );
        wp_enqueue_style( 'wpbugtracktor' );                    
        
        $output = '<div class="wpBugTracktor">

        ';
        $display = strtolower($display);
        switch ($display) {

                case 'mainpage' : // mainpage shortcode =========================================================
                    if(@!isset($_GET['list_issues_from_project'])) {
                        wpBugTracktorShowPublicProjects($primkey);
                        wpBugTracktorReportAnIssueStep1();
                    } else {
                        $project_id = intval($_GET['list_issues_from_project']);
                        wpBugTracktorViewPublicIssues($project_id);
                    }
                break;

                case 'report': // Report shortcode =========================================================
                    wpBugTracktorReportAnIssueStep1();
                break;                
            
                case 'issues' : // issues shortcode =========================================================
                    wpBugTracktorReportAnIssueStep2($primkey);
                break;             
            
                case 'listissues' : // View all issues, optionally for a specific project =========================================================
                    wpBugTracktorViewPublicIssues($primkey);
                break;             
            
                case 'projects' : // list projects shortcode =========================================================
                    wpBugTracktorShowPublicProjects();
                break;            
            
                case 'roadmap': // roadmap & release log shortcode =========================================================

                break;            
 
                case 'statistics': // statistics shortcode =========================================================

                break;               
            
        }
        $output .= '</div>';

        return do_shortcode($output);
        

    }
}




add_shortcode('wpbugtracktor', 'wpBugTracktorMainShortcode');


?>