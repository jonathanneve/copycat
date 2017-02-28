<?php

if(!function_exists('wpBugTracktorAdminPageHeader')) {
    function wpBugTracktorAdminPageHeader() {
        echo '
        <img src="'.plugins_url().'/wpbugtracktor/images/logo.png" alt="wpBugTracktor" /><br />
            ';
        wpBugTracktorCheckAdminPermissions();
    }
}

if(!function_exists('wpBugTracktorAdminPageMain')) {
    function wpBugTracktorAdminPageMain() {
        wpBugTracktorAdminPageHeader();
        echo '
        <div style="padding:10px;font-size:1.7em;">
            <h2>'.__('Welcome to Our BETA Test', 'wpbugtracktor').'</h2>
            <table class="widefat"><tr><td>'.__('wpbugtracktor is an open source issue tracking solution for Wordpress.  The software is fresh out the oven, and really not ready for production.  This is early beta software.  As in, we haven\'t even finished implementing all of our basic features yet.  Don\'t worry, we\'re going to take care of that very quickly.  Within a month or two this software will be feature complete and ready for the good old 1.0 stable production release... at least that\'s that goal.  You can help!  Give us feedback, bug reports, feature requests, and any help or encouragement you can spare.  Thanks for checking out our software; we hope you enjoy it!', 'wpbugtracktor').'</td></tr></table>
            <br />
            <h2>'.__('Beta Test Feedback', 'wpbugtracktor').'</h2>
            <table class="widefat"><tr><td><center><a href="http://wpbugtracktor.com/bug-tracker/?issue_tracker=bug&wpbt_project=5" target="_blank"><button> <center><img src="' . plugins_url() . '/wpbugtracktor/images/bug_report.png" alt="" /><br /> '.__('Report a bug', 'wpbugtracktor').'</center></button></a><a href="http://wpbugtracktor.com/bug-tracker/?issue_tracker=feature&wpbt_project=5" target="_blank"><button> <center><img src="' . plugins_url() . '/wpbugtracktor/images/feature_request.png" alt="" /><br /> '.__('Feature request', 'wpbugtracktor').'</center></button></a></center></td></tr></table>
            
        </div>';
    }
}

if(!function_exists('wpBugTracktorAdminPageAddNewProject')) {
    function wpBugTracktorAdminPageAddNewProject() {
        $wpBugTracktorOptions = get_option('wpBugTracktorAdminOptions');   
        wpBugTracktorAdminPageHeader();
        echo '
        <div style="padding:10px;">
            <form action="admin.php?page=wpbugtracktor-edit-projects" method="post" />
                <h1>'.__('Add New Project', 'wpbugtracktor').'</h1>
                <table class="widefat">
                    <thead>
                        <tr><th style="width:20%;"></th><th>'.__('Value', 'wpbugtracktor').'</th></tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td style="width:20%;">
                                <h2>'.__('Project Title', 'wpbugtracktor').'</h2>
                                <p>'.__('The public name of the project, without version numbers or edition names.', 'wpbugtracktor').'</p>
                            </td>
                            <td>
                                <br />
                                <input type="text" value="" name="wpbt_new_project_title" id="wpbt_new_project_title" style="width:100%;height:34px;font-size:24px;" />
                            </td>                            
                        </tr>
                        <tr>
                            <td style="width:20%;">
                                <h2>'.__('Description', 'wpbugtracktor').'</h2>
                                <p>'.__('The public description of the project, which should summarize what the project is all about.', 'wpbugtracktor').'</p>
                            </td>
                            <td>
                                <textarea name="wpbt_new_project_description" id="wpbt_new_project_description" style="width:100%;height:86px;font-size:14px;"></textarea>
                            </td>                            
                        </tr>     
                        <tr>
                            <td style="width:20%;">
                                <h2>'.__('Project Prefix', 'wpbugtracktor').'</h2>
                                <p>'.__('Each issue is assigned a number, and this, the project prefix, is appended to the beginning of the issue number.', 'wpbugtracktor').'</p>
                            </td>
                            <td>
                                <br />
                                <input type="text" value="" name="wpbt_new_project_prefix" id="wpbt_new_project_prefix" style="width:100px;height:34px;font-size:24px;" />
                            </td>                            
                        </tr>    
                        <tr>
                            <td style="width:20%;">
                                <h2>'.__('Project Leader', 'wpbugtracktor').'</h2>
                                <p>'.__('This is the user in charge of the project, who also manages any other users assigned to this project.', 'wpbugtracktor').'</p>
                            </td>
                            <td><br /><select name="wpbt_new_project_owner_id" id="wpbt_new_project_owner_id">';
                                
                            $wpbt_managers = wpBugTracktorReturnValidManagers();
                            foreach ($wpbt_managers as $wpbt_manager) {
                                global $user_info;
                                $user_info = get_userdata($wpbt_manager);
                                echo '<option value="'.$wpbt_manager.'">'.$user_info->user_login.'</option>';
                            }
        
                            echo '</select>
                            </td>                            
                        </tr>                         
                    </tbody>
                </table>
            <br />
            <input type="submit" class="button-primary" value="'.__('Create New Project','wpbugtracktor').'" />
            </form>
        </div>
        ';
        
    }
}

if(!function_exists('wpBugTracktorAdminPageEditProjects')) {
    function wpBugTracktorAdminPageEditProjects() {
        global $wpdb;
        //$wpBugTracktorOptions = get_option('wpBugTracktorAdminOptions');   
        wpBugTracktorAdminPageHeader();
        
        $wpBugTracktorOptions = get_option('wpBugTracktorAdminOptions');
        
        if(@isset($_POST['wpbt_new_project_prefix']) && @isset($_POST['wpbt_new_project_title']) && @isset($_POST['wpbt_new_project_description'])) {
            $prefix = $wpdb->escape($_POST['wpbt_new_project_prefix']);
            $title = $wpdb->escape($_POST['wpbt_new_project_title']);
            $description = $wpdb->escape($_POST['wpbt_new_project_description']);
            $owner_id = $wpdb->escape($_POST['wpbt_new_project_owner_id']);
            $sql = "INSERT INTO `{$wpdb->prefix}wpbugtracktor_projects` (`primkey`, `title`, `description`, `project_prefix`, `owner_id`, `users_assigned`) VALUES (NULL, '{$title}', '{$description}', '{$prefix}', '{$owner_id}', '1');";
            $wpdb->query($sql);
        }
        
        echo "
            <style type=\"text/css\">
                .edit, .wpbt-edit-user {cursor:pointer;opacity:0.8;}
                .edit:hover, .wpbt-edit-user:hover {opacity:1.0;}
            </style>
            <script type=\"text/javascript\">";
            echo ' 
                function wpbtSaveNewComponent() {
                    jQuery.post(ajaxurl, { action: \'wpbt_save_component\' , projectid: jQuery(\'#wpbt-add-new-component-form-project-id\').val(), title: jQuery(\'#wpbt-add-new-component-form-title\').val(),  desc: jQuery(\'#wpbt-add-new-component-form-description\').val() }, function(data) {
                        jQuery("<div id=\"wpbt_component_"+data+"\"><img src=\"'.plugins_url().'/wpbugtracktor/images/x_alt_24x24.png\" style=\"cursor:pointer;margin:4px;float:left;\" onclick=\"if ( confirm(\''.__('Are you sure you wish to delete this component?', 'wpbugtracktor').'\') ) { jQuery.post( ajaxurl+\'?action=wpbt_delete_component\', { primkey: "+data+"}, function(data) { jQuery(\'#wpbt_component_"+data+"\').remove(); });  }\" /> <div class=\"edit\" id=\"componentTitle_"+data+"\" style=\"float:left;\">"+jQuery(\'#wpbt-add-new-component-form-title\').val()+"</div> <br /><span class=\"edit\" id=\"componentDescription_"+data+"\" style=\"font-size:0.8em;\">"+jQuery(\'#wpbt-add-new-component-form-description\').val()+"</span><br /><br /></div>").appendTo("#wpbt_components_project_" + jQuery(\'#wpbt-add-new-component-form-project-id\').val());
                        jQuery(\'#wpbt-add-new-component-form-project-id\').val(\'0\');
                        jQuery(\'#wpbt-add-new-component-form-title\').val(\'\');
                        jQuery(\'#wpbt-add-new-component-form-description\').val(\'\'); 
                        tb_remove();
                        jQuery(".edit").editable(ajaxurl+"?action=wpbt_save_project", { 
                            type      : "text",
                            width     : "180px",
                            height    : "20px",                            
                            cancel    : "'.__('Cancel', 'wpbugtracktor').'",
                            submit    : "'.__('Save', 'wpbugtracktor').'",
                            tooltip   : "'.__('Click to Edit', 'wpbugtracktor').'"
                        });                        
                    }); 
                }
                ';
                echo " 
                jQuery(document).ready(function() {
                    jQuery('.wpbt-edit').editable(ajaxurl+'?action=wpbt_save_project', { 
                        type      : 'text',
                            width     : '180px',
                            height    : '20px',                        
                        cancel    : '".__('Cancel', 'wpbugtracktor')."',
                        submit    : '".__('Save', 'wpbugtracktor')."',
                        tooltip   : '".__('Click to Edit', 'wpbugtracktor')."'
                    });
                    
                    jQuery('.wpbt-edit-user').editable(ajaxurl+'?action=wpbt_save_project', {
                        submit : \"".__('Save', 'wpbugtracktor')."\",
                        cancel : \"".__('Cancel', 'wpbugtracktor')."\",
                        type   : \"select\",
                        data   : \" {'0':'".__('Unassigned', 'wpbugtracktor')."'"; 
                                        global $blog_id; 
                                        $wpscBlogUsers = get_users("blog_id={$blog_id}&orderby=nicename&role={$wpBugTracktorOptions['admin_capability']}");
                                        if(isset($wpscBlogUsers[0])) {
                                            foreach ($wpscBlogUsers as $wpscTempUser) {
                                                echo  ",'{$wpscTempUser->ID}' : '".htmlentities($wpscTempUser->display_name)."' ";
                                            }         
                                        }
                                        echo '}"

                    });
                    
                 });
            </script>

        <div id="wpbt-add-new-component-content-id" style="display:none;">
             <div>
                  <h1>'.__('Create a New Component', 'wpbugtracktor').'</h1>
                  '.__('Create a new component for this project.  Think of components as completely optional sub-projects.  For example, a video game might have a Graphical component, a Network component, a Quest System component, and more.  If your project has large, but relatively unrelated sections to them, you can optionally use Components to isolate bugs within those specific sections.', 'wpbugtracktor').'<br /><br />
                      <form id="wpbt-add-new-component-form" name="" type="post" action="#">
                      <table class="widefat">
                        <tr><td></td><td><input type="hidden" value="" id="wpbt-add-new-component-form-project-id" /></td></tr>
                        <tr><td>'.__('Component Name/Title', 'wpbugtracktor').'</td><td><input type="text" value="" id="wpbt-add-new-component-form-title" /></td></tr>
                        <tr><td>'.__('Description', 'wpbugtracktor').'</td><td><input type="text" value="" id="wpbt-add-new-component-form-description" /></td></tr>
                      </table>
                      <br /><a href="#" class="button-secondary" onclick="wpbtSaveNewComponent();return false;" style="float:right;"><img src="'.plugins_url().'/wpbugtracktor/images/buttons_19.png" alt="" style="position:relative;top:4px;" /> '.__('Save New Component', 'wpbugtracktor').'</a>
                      </form>
             </div>
        </div>

            ';
        
        $wpbtProjects = wpBugTracktorGetProjects();
        echo '        <div style="padding:10px;">
            <form action="" method="post" />
                <h1>'.__('Edit Projects', 'wpbugtracktor').'</h1>
                
                <a class="button-secondary" href="admin.php?page=wpbugtracktor-new-project" style="float:right;position:relative;top:-40px;">'.__('Add New Project', 'wpbugtracktor').'</a>   
                <table class="widefat">
                    <thead>
                        <tr><th style="width:20%;">'.__('Key', 'wpbugtracktor').'</th><th>'.__('Title', 'wpbugtracktor').'</th><th>'.__('Description', 'wpbugtracktor').'</th><th>'.__('Prefix', 'wpbugtracktor').'</th><th>'.__('Owner', 'wpbugtracktor').'</th><th>'.__('Components','wpbugtracktor').'</th></tr>
                    </thead><tbody>';
        foreach ($wpbtProjects as $wpbtProject) {
            global $user_info;
            $user_info = get_userdata($wpbtProject['owner_id']);
            
            echo '<tr id="wpbt_project_'.$wpbtProject['primkey'] .'"><td><img src="'.plugins_url().'/wpbugtracktor/images/x_alt_24x24.png" style="cursor:pointer;" onclick="if ( confirm(\''.__('Are you sure you wish to delete this project?', 'wpbugtracktor').'\') ) { jQuery.post(ajaxurl+\'?action=wpbt_delete_project\', { primkey: '.$wpbtProject['primkey'] .'}, function(data) { jQuery(\'#wpbt_project_'.$wpbtProject['primkey'] .'\').remove(); });  }" /> '.$wpbtProject['primkey'] .'</td><td class="wpbt-edit" id="title_'.$wpbtProject['primkey'] .'">'. $wpbtProject['title'] .'</td><td class="wpbt-edit" id="description_'.$wpbtProject['primkey'] .'">'.  $wpbtProject['description'] .'</td><td class="wpbt-edit" id="prefix_'.$wpbtProject['primkey'] .'">'.  $wpbtProject['project_prefix'] .'</td><td class="wpbt-edit-user"  id="owner_'.$wpbtProject['primkey'] .'">'.  $user_info->user_login . '</td><td>';
            $componentresults = $wpdb->get_results("SELECT * FROM `{$wpdb->prefix}wpbugtracktor_components` WHERE `project_id`='{$wpbtProject['primkey']}';",ARRAY_A);
            add_thickbox();
            echo '<a href="#TB_inline?width=500&height=400&inlineId=wpbt-add-new-component-content-id" class="thickbox button-secondary" onclick="jQuery(\'#wpbt-add-new-component-form-project-id\').val(\''.$wpbtProject['primkey'].'\');"><img src="'.plugins_url().'/wpbugtracktor/images/buttons_19.png" alt="" style="position:relative;top:4px;" /> '.__('Add New Component to this Project', 'wpbugtracktor').'</a><br /><br /><div id="wpbt_components_project_'.$wpbtProject['primkey'] .'">';
            if(@isset($componentresults[0]['primkey'])) {
                foreach($componentresults as $componentresult) {
                    echo '<div id="wpbt_component_'.$componentresult['primkey'].'"><img src="'.plugins_url().'/wpbugtracktor/images/x_alt_24x24.png" style="cursor:pointer;margin:4px;float:left;" onclick="if ( confirm(\''.__('Are you sure you wish to delete this component?', 'wpbugtracktor').'\') ) { jQuery.post(ajaxurl+\'?action=wpbt_delete_component\', { primkey: '.$componentresult['primkey'] .'}, function(data) { jQuery(\'#wpbt_component_'.$componentresult['primkey'] .'\').remove(); });  }" /> ';
                    echo '<div class="wpbt-edit" id="componentTitle_'.$componentresult['primkey'].'" style="float:left;">'.$componentresult['title'] . '</div><br /><span style="font-size:0.8em;" class="wpbt-edit" id="componentDescription_'.$componentresult['primkey'].'">'.$componentresult['description'].'</span><br /><br /></div>';
                }
            }
            echo '</div></td></tr>';
        }
        echo '                    </tbody>
                </table>  
                <br />';
        
    }
}

if(!function_exists('wpBugTracktorAdminPageVersions')) {
    function wpBugTracktorAdminPageVersions() {
        global $wpdb;
        
        $wpBugTracktorOptions = get_option('wpBugTracktorAdminOptions');   
        wpBugTracktorAdminPageHeader();
        echo '
        <style>

            /* the input field */
            .date {
                border:1px solid #ccc;
                font-size:18px;
                padding:4px;
                text-align:center;
                width:194px;

                -moz-box-shadow:0 0 10px #eee inset;
                -webkit-box-shadow:0 0 10px #eee inset;
            }

            /* calendar root element */
            #calroot {
                /* place on top of other elements. set a higher value if nessessary */
                z-index:10000;
                position:absolute;
                margin-top:-1px;
                width:198px;
                padding:2px;
                background-color:#fff;
                font-size:11px;
                border:1px solid #ccc;

                -moz-border-radius:5px;
                -webkit-border-radius:5px;

                -moz-box-shadow: 0 0 15px #666;
                -webkit-box-shadow: 0 0 15px #666;
            }

            /* head. contains title, prev/next month controls and possible month/year selectors */
            #calhead {
                padding:2px 0;
                height:22px;
            }

            #caltitle {
                font-size:14px;
                color:#0150D1;
                float:left;
                text-align:center;
                width:155px;
                line-height:20px;
                text-shadow:0 1px 0 #ddd;
            }

            #calnext, #calprev {
                display:block;
                width:20px;
                height:20px;
                background:transparent url('.plugins_url().'/wpbugtracktor/images/prev.png) no-repeat scroll center center;
                float:left;
                cursor:pointer;
            }

            #calnext {
                background-image:url('.plugins_url().'/wpbugtracktor/images/next.png);
                float:right;
            }

            #calprev.caldisabled, #calnext.caldisabled {
                visibility:hidden;
            }

            /* year/month selector */
            #caltitle select {
                font-size:10px;
            }

            /* names of the days */
            #caldays {
                height:14px;
                border-bottom:1px solid #ddd;
            }

            #caldays span {
                display:block;
                float:left;
                width:28px;
                text-align:center;
            }

            /* container for weeks */
            #calweeks {
                background-color:#fff;
                margin-top:4px;
            }

            /* single week */
            .calweek {
                clear:left;
                height:22px;
            }

            /* single day */
            .calweek a {
                display:block;
                float:left;
                width:27px;
                height:20px;
                text-decoration:none;
                font-size:11px;
                margin-left:1px;
                text-align:center;
                line-height:20px;
                color:#666;
                -moz-border-radius:3px;
                -webkit-border-radius:3px;
            }

            /* different states */
            .calweek a:hover, .calfocus {
                background-color:#ddd;
            }

            /* sunday */
            a.calsun {
                color:red;
            }

            /* offmonth day */
            a.caloff {
                color:#ccc;
            }

            a.caloff:hover {
                background-color:rgb(245, 245, 250);
            }


            /* unselecteble day */
            a.caldisabled {
                background-color:#efefef !important;
                color:#ccc	!important;
                cursor:default;
            }

            /* current day */
            #calcurrent {
                background-color:#498CE2;
                color:#fff;
            }

            /* today */
            #caltoday {
                background-color:#333;
                color:#fff;
            }        
        </style>
        <script type="text/javascript">
            jQuery(document).ready(function() {
                jQuery(":date").dateinput({format : "yyyymmdd"});
                jQuery("body").on("mousedown", ".date", function(event) {
                    jQuery("#calroot").css("top", event.pageY);
                    jQuery("#calroot").css("left", event.pageX);
                });
            });
            
            function wpbtMakeNewRelease() {
                jQuery.post(ajaxurl+"?action=wpbt_save_milestone", { wpbt_version: jQuery(\'#wpbt_version\').val(), wpbt_codename: jQuery(\'#wpbt_codename\').val(), wpbt_desc: jQuery(\'#wpbt_desc\').val(), wpbt_project: jQuery(\'#wpbt_project\').val(), wpbt_isreleased: jQuery(\'#wpbt_isreleased\').val(), wpbt_startdate: jQuery(\'#wpbt_startdate\').val(), wpbt_releasedate: jQuery(\'#wpbt_releasedate\').val() }, function(data) {
                    alert("'.__('Saved successfully.','wpbugtracktor').'");
                    location.reload();
                });
                
                return false;
            }
            
            function wpbtEditReleases() {
                jQuery(".wpbt_edit_primkey").each(function( index ) {
                    jQuery.post(ajaxurl+"?action=wpbt_save_milestone", { wpbt_primkey:jQuery(this).val(), wpbt_version: jQuery(\'#wpbt_version_\'+jQuery(this).val()).val(), wpbt_codename: jQuery(\'#wpbt_codename_\'+jQuery(this).val()).val(), wpbt_desc: jQuery(\'#wpbt_desc_\'+jQuery(this).val()).val(), wpbt_project: jQuery(\'#wpbt_project_\'+jQuery(this).val()).val(), wpbt_isreleased: jQuery(\'#wpbt_isreleased_\'+jQuery(this).val()).val(), wpbt_startdate: jQuery(\'#wpbt_startdate_\'+jQuery(this).val()).val(), wpbt_releasedate: jQuery(\'#wpbt_releasedate_\'+jQuery(this).val()).val() }, function(data) {
                         
                    });                
                });
                alert("'.__('Save successfully.','wpbugtracktor').'");
                return false;
            }

        </script>            
        <div style="padding:10px;">';
        
        echo '
       <form name="wpbt_new_release" method="post" onsubmit="return false;">
            <table class="widefat">
                <thead><tr><th>
                    <h2>'.__('Make a new release (Milestones, Versions, Editions, etc.)', 'wpbugtracktor').'</h2>
                    <p>'.__('Here you can create a new release for any of your projects.  A release can be anything, including but not limited to milestones, major releases (1.x), minor releases (1.1), prototypes, alphas, betas, platform releases, or any other division or revision of your project.', 'wpbugtracktor').'</p>
                </th></tr></thead>
                <tbody><tr><td>
                <table class="widefat">
                    <thead><tr><th>'.__('Version Number', 'wpbugtracktor').'</th><th>'.__('Version Name/Codename', 'wpbugtracktor').'</th><th>'.__('Description', 'wpbugtracktor').'</th><th>'.__('Project', 'wpbugtracktor').'</th><th>'.__('Is It Currently Released?', 'wpbugtracktor').'</th><th>'.__('Start Date', 'wpbugtracktor').'</th><th>'.__('Release Date', 'wpbugtracktor').'</th></tr></thead>
                    <tbody><tr><td><input type="text" name="wpbt_version" id="wpbt_version" style="width:70px;" /></td><td><input type="text" name="wpbt_codename" id="wpbt_codename" /></td><td><input type="text" name="wpbt_desc" id="wpbt_desc" /></td>';
                    echo '<td>';
                    $results = $wpdb->get_results("SELECT `primkey`, `title` FROM `{$wpdb->prefix}wpbugtracktor_projects`; ",ARRAY_A);
                    if(@isset($results[0]['title'])) {
                        echo '<select name="wpbt_project" id="wpbt_project">';
                        foreach ($results as $result) {
                            echo '<option value="'.$result['primkey'].'">'.$result['title'].'</option>';
                        }
                        echo '</select>';
                    }
                    echo '</td>';
                    echo '<td><select name="wpbt_isreleased" id="wpbt_isreleased"><option value="0">'.__('Not released yet', 'wpbugtracktor').'</option><option value="1">'.__('Yes, already released', 'wpbugtracktor').'</option></select></td><td><input type="date" class="date" name="wpbt_startdate" id="wpbt_startdate" style="width:120px;"  value="'.date('Y-m-d').'"   /></td><td><input type="date" class="date" name="wpbt_releasedate" id="wpbt_releasedate" style="width:120px;" value="'.date('Y-m-d').'" />
                </td></tr></tbody>
                </table>';
        echo '<br /><button class="button-primary" onclick="wpbtMakeNewRelease();return false;">'.__('Create New Release', 'wpbugtracktor').'</button>
            </td></tr></table>
        </form><br />';
        
        echo '
            <table class="widefat">
                <thead><tr><th>
                    <h2>'.__('Edit &amp; Manage Releases:', 'wpbugtracktor').'</h2>
                    <p>'.__('Here you can edit or delete previous releases.', 'wpbugtracktor').'</p>
                </th></tr></thead>
                <tbody><tr><td>
                <table class="widefat">';
        
       echo '<thead><tr><th>'.__('Version Number', 'wpbugtracktor').'</th><th>'.__('Version Name/Codename', 'wpbugtracktor').'</th><th>'.__('Description', 'wpbugtracktor').'</th><th>'.__('Project', 'wpbugtracktor').'</th><th>'.__('Is Currently Released?', 'wpbugtracktor').'</th><th>'.__('Start Date', 'wpbugtracktor').'</th><th>'.__('Release Date', 'wpbugtracktor').'</th></tr></thead>'; 

       $msresults = $wpdb->get_results("SELECT * FROM `{$wpdb->prefix}wpbugtracktor_milestones` ORDER BY `project_id`, `version_number`;",ARRAY_A);
       if(@isset($msresults[0]['primkey'])) {
           foreach ($msresults as $msresult) {
               echo '<tr><td><form name="wpbt_edit_release_'.$msresult['primkey'].'" class="wpbt_edit_release_form" method="post" onsubmit="return false;"><input type="hidden" name="wpbt_edit_primkey" class="wpbt_edit_primkey" value="'.$msresult['primkey'].'" /><input type="text" name="wpbt_version" id="wpbt_version_'.$msresult['primkey'].'" value="'.$msresult['version_number'].'"  style="width:70px;" /></td><td><input type="text" name="wpbt_codename" id="wpbt_codename_'.$msresult['primkey'].'" value="'.$msresult['title'].'"  /></td><td><input type="text" name="wpbt_desc" id="wpbt_desc_'.$msresult['primkey'].'" value="'.$msresult['description'].'"  /></td>';
                    echo '<td>';
                    if(@isset($results[0]['title'])) {
                        echo '<select name="wpbt_project" id="wpbt_project_'.$msresult['primkey'].'">';
                        foreach ($results as $result) {
                            $project_selection = '';
                            if($result['primkey'] == $msresult['project_id']) {
                                $project_selection = ' selected ';
                            }
                            echo '<option value="'.$result['primkey'].'" '.$project_selection.'>'.$result['title'].'</option>';
                        }
                        echo '</select>';
                    }
                    echo '</td>';
                    $is_released_selection = '';
                    if($msresult['is_released'] == 1) {
                        $is_released_selection = ' selected ';
                    }
                    $start_date = str_split($msresult['start_date'], 2);
                    $release_date = str_split($msresult['release_date'], 2);
                    echo '<td><select name="wpbt_isreleased" id="wpbt_isreleased_'.$msresult['primkey'].'"><option value="0">'.__('Not released yet', 'wpbugtracktor').'</option><option value="1" '.$is_released_selection.'>'.__('Yes, already released', 'wpbugtracktor').'</option></select></td><td><input type="date" class="date" name="wpbt_startdate_'.$msresult['primkey'].'" id="wpbt_startdate_'.$msresult['primkey'].'" style="width:120px;"  value="'.$start_date[0].$start_date[1].'-'.$start_date[2].'-'.$start_date[3].'"  /></td><td><input type="date" class="date" name="wpbt_releasedate_'.$msresult['primkey'].'" id="wpbt_releasedate_'.$msresult['primkey'].'" style="width:120px;"  value="'.$release_date[0].$release_date[1].'-'.$release_date[2].'-'.$release_date[3].'"  />
                </form></td></tr>';
                
                
           }
       }
       echo '</td></tr></tbody></table>';
       echo '<br /><button class="button-primary" onclick="wpbtEditReleases();return false;">'.__('Save All Edits', 'wpbugtracktor').'</button></td></tr></tbody></table>';
        
        
        echo '</div>';
    }
}

if(!function_exists('wpBugTracktorAdminPageManageComments')) {
    function wpBugTracktorAdminPageManageComments() {
        $wpBugTracktorOptions = get_option('wpBugTracktorAdminOptions');   
        wpBugTracktorAdminPageHeader();
        
        wpBugTracktorViewAdminIssueComments(null, true);
    }
}

if(!function_exists('wpBugTracktorAdminPageIssues')) {
    function wpBugTracktorAdminPageIssues() {
        global $wpdb;
        
        $wpBugTracktorOptions = get_option('wpBugTracktorAdminOptions');   
        wpBugTracktorAdminPageHeader();
        $issues = wpBugTracktorGetIssues('*', ";");
        if(@isset($issues[0]['primkey'])) {
            
            $wpBugTracktorOptions['toggle_issue_primkey'];
            $wpBugTracktorOptions['toggle_issue_title'];
            $wpBugTracktorOptions['toggle_issue_description'];
            $wpBugTracktorOptions['toggle_issue_type'];
            $wpBugTracktorOptions['toggle_issue_status'];
            $wpBugTracktorOptions['toggle_issue_reporter_id'];
            $wpBugTracktorOptions['toggle_issue_reporter_email'];
            $wpBugTracktorOptions['toggle_issue_reporter_ip'];
            $wpBugTracktorOptions['toggle_issue_owner_id'];
            $wpBugTracktorOptions['toggle_issue_project_id'];
            $wpBugTracktorOptions['toggle_issue_project_component_id'];
            $wpBugTracktorOptions['toggle_issue_severity_priority'];
            $wpBugTracktorOptions['toggle_issue_version_reported'];
            $wpBugTracktorOptions['toggle_issue_target_fix_for_milestone_id'];
            $wpBugTracktorOptions['toggle_issue_tags'];
            $wpBugTracktorOptions['toggle_issue_date_reported'];
            $wpBugTracktorOptions['toggle_issue_date_last_modified'];
            $wpBugTracktorOptions['toggle_issue_post_id'];
            $wpBugTracktorOptions['toggle_issue_related_issue_ids'];
            $wpBugTracktorOptions['toggle_issue_email_updates_to_these_users'];
            $wpBugTracktorOptions['toggle_issue_attachments'];            
            
            echo ' 
            <script type="text/javascript">
            
                var uTable;

                function wpbtPublish(page_id) {
                    jQuery.post(ajaxurl+"?action=wpbt_save_publish", { wpBugTracktorPublish: \'publish\', wpBugTracktorPublishPageId: page_id }, function(data) {
                        location.reload();        
                    });
                                 
                }
                
                function wpbtUnpublish(page_id) {
                    jQuery.post(ajaxurl+"?action=wpbt_save_publish", { wpBugTracktorPublish: \'draft\', wpBugTracktorPublishPageId: page_id }, function(data) {
                        location.reload();    
                    });
                              
                }

                function wpBugTracktorShowHide( iCol, oTable ) {
                    var bVis = oTable.fnSettings().aoColumns[iCol].bVisible;
                    oTable.fnSetColumnVis( iCol, bVis ? false : true );
                    oTable.fnAdjustColumnSizing();
                    oTable.fnDraw(); 
                }


                function wpbtChangeSetting(wpBugTracktorChangeSetting, wpBugTracktorChangeSettingValue) {
                    jQuery.post(ajaxurl+"?action=wpbt_save_setting", { wpBugTracktorChangeSetting: wpBugTracktorChangeSetting, wpBugTracktorChangeSettingValue: wpBugTracktorChangeSettingValue }, function(data) {

                    });

                    return false;
                }
                
                function wpbtToggleViewSetting(wpBugTracktorChangeSetting, checkboxElement) {
                    var wpBugTracktorChangeSettingValue;
                    if(!checkboxElement.checked) {
                        wpBugTracktorChangeSettingValue = "true";
                    } else {
                        wpBugTracktorChangeSettingValue = "false";
                    }
                    wpbtChangeSetting(wpBugTracktorChangeSetting, wpBugTracktorChangeSettingValue);
                }
                

                function wpbtInitTable() {
                
                    uTable = jQuery("#wpBugTracktorAdminIssueTable").dataTable( {
                            "bScrollInfinite": true,
                            "bScrollCollapse": true,
                            "sServerMethod": "POST",
                            "bDeferRender": true,                            
                            "sScrollY": "600px",
                            "bProcessing": true,
                            "sAjaxSource": ajaxurl+\'?action=wpbt_json_issues\',
                            "fnDrawCallback": function () {
                            
                                jQuery(".wpbt-edit").editable(ajaxurl+"?action=wpbt_edit_issue", {
                                    "indicator" : "<img src=\\"'. plugins_url().'/wpbugtracktor/images/loader.gif\\">",
                                    "callback": function( sValue, y ) {
                                        uTable.fnAdjustColumnSizing();
                                        uTable.fnDraw();
                                    },
                                    "submitdata": function ( value, settings ) {
                                        return {
                                            "row_id": jQuery(this).parent().parent().attr("id"),
                                            "column": uTable.fnGetPosition( this.parentNode )[2]
                                        };
                                    },                                
                                    "height": "14px"
                                });

                                jQuery(".wpbt-edit-type").editable(ajaxurl+"?action=wpbt_edit_issue", {
                                    "indicator" : "<img src=\\"'. plugins_url().'/wpbugtracktor/images/loader.gif\\">",
                                    "callback": function( sValue, y ) {
                                        uTable.fnAdjustColumnSizing();
                                        uTable.fnDraw();
                                    },
                                    "submitdata": function ( value, settings ) {
                                        return {
                                            "row_id": jQuery(this).parent().parent().attr("id"),
                                            "column": uTable.fnGetPosition( this.parentNode )[2]
                                        };
                                    },                                     
                                    "submit" : "'.__('Save', 'wpbugtracktor').'",
                                    "cancel" : "'.__('Cancel', 'wpbugtracktor').'",
                                    "type"   : "select",
                                    "data"   : " {\'0\' : \''.__('Bug', 'wpbugtracktor').'\', \'1\' : \''.__('Feature Request', 'wpbugtracktor').'\', \'2\' : \''.__('Regression', 'wpbugtracktor').'\',  \'3\' : \''.__('Enhancement', 'wpbugtracktor').'\',  \'4\' : \''.__('Idea', 'wpbugtracktor').'\' } "

                                });
                                
                                jQuery(".wpbt-edit-severity").editable(ajaxurl+"?action=wpbt_edit_issue", {
                                    "indicator" : "<img src=\\"'. plugins_url().'/wpbugtracktor/images/loader.gif\\">",
                                    "callback": function( sValue, y ) {
                                        uTable.fnAdjustColumnSizing();
                                        uTable.fnDraw();
                                    },
                                    "submitdata": function ( value, settings ) {
                                        return {
                                            "row_id": jQuery(this).parent().parent().attr("id"),
                                            "column": uTable.fnGetPosition( this.parentNode )[2]
                                        };
                                    },                                     
                                    "submit" : "'.__('Save', 'wpbugtracktor').'",
                                    "cancel" : "'.__('Cancel', 'wpbugtracktor').'",
                                    "type"   : "select",
                                    "data"   : " {\'0\' : \''.__('Non-critical', 'wpbugtracktor').'\', \'1\' : \''.__('Low priority', 'wpbugtracktor').'\', \'2\' : \''.__('Medium', 'wpbugtracktor').'\',  \'3\' : \''.__('Above Average', 'wpbugtracktor').'\',  \'4\' : \''.__('Critical', 'wpbugtracktor').'\' ,  \'5\' : \''.__('Emergency', 'wpbugtracktor').'\' } "

                                });
                                
                                jQuery(".wpbt-edit-version").editable(ajaxurl+"?action=wpbt_edit_issue", {
                                    "indicator" : "<img src=\\"'. plugins_url().'/wpbugtracktor/images/loader.gif\\">",
                                    "callback": function( sValue, y ) {
                                        uTable.fnAdjustColumnSizing();
                                        uTable.fnDraw();
                                    },
                                    "submitdata": function ( value, settings ) {
                                        return {
                                            "row_id": jQuery(this).parent().parent().attr("id"),
                                            "column": uTable.fnGetPosition( this.parentNode )[2]
                                        };
                                    },                                     
                                    "submit" : "'.__('Save', 'wpbugtracktor').'",
                                    "cancel" : "'.__('Cancel', 'wpbugtracktor').'",
                                    "type"   : "select",
                                    "data"   : " {\'0\':\''.__('Unassigned', 'wpbugtracktor').'\' ';
                                        $version_results = $wpdb->get_results("SELECT * FROM `{$wpdb->prefix}wpbugtracktor_milestones`;", ARRAY_A);
                                        if(isset($version_results[0]['primkey'])) {
                                            foreach ($version_results as $version_result) {
                                                echo  ",'{$version_result['primkey']}' : '".htmlentities($version_result['title'])."' ";
                                            }         
                                        }
                                        echo '}"

                                });

                                jQuery(".wpbt-edit-component").editable(ajaxurl+"?action=wpbt_edit_issue", {
                                    "indicator" : "<img src=\\"'. plugins_url().'/wpbugtracktor/images/loader.gif\\">",
                                    "callback": function( sValue, y ) {
                                        uTable.fnAdjustColumnSizing();
                                        uTable.fnDraw();
                                    },
                                    "submitdata": function ( value, settings ) {
                                        return {
                                            "row_id": jQuery(this).parent().parent().attr("id"),
                                            "column": uTable.fnGetPosition( this.parentNode )[2]
                                        };
                                    },                                     
                                    "submit" : "'.__('Save', 'wpbugtracktor').'",
                                    "cancel" : "'.__('Cancel', 'wpbugtracktor').'",
                                    "type"   : "select",
                                    "data"   : " {\'0\':\''.__('None', 'wpbugtracktor').'\' ';
                                        $component_results = $wpdb->get_results("SELECT * FROM `{$wpdb->prefix}wpbugtracktor_components`;", ARRAY_A);
                                        if(isset($component_results[0]['primkey'])) {
                                            foreach ($component_results as $component_result) {
                                                echo  ",'{$component_result['primkey']}' : '".htmlentities($component_result['title'])."' ";
                                            }         
                                        }
                                        echo '}"

                                });

                                jQuery(".wpbt-edit-project").editable(ajaxurl+"?action=wpbt_edit_issue", {
                                    "indicator" : "<img src=\\"'. plugins_url().'/wpbugtracktor/images/loader.gif\\">",
                                    "callback": function( sValue, y ) {
                                        uTable.fnAdjustColumnSizing();
                                        uTable.fnDraw();
                                    },
                                    "submitdata": function ( value, settings ) {
                                        return {
                                            "row_id": jQuery(this).parent().parent().attr("id"),
                                            "column": uTable.fnGetPosition( this.parentNode )[2]
                                        };
                                    },                                     
                                    "submit" : "'.__('Save', 'wpbugtracktor').'",
                                    "cancel" : "'.__('Cancel', 'wpbugtracktor').'",
                                    "type"   : "select",
                                    "data"   : " {\'0\':\''.__('Unassigned', 'wpbugtracktor').'\' ';
                                        $project_results = wpBugTracktorGetProjects($selection=' `primkey`, `title` ', ';');
                                        if(isset($project_results[0]['primkey'])) {
                                            foreach ($project_results as $project_result) {
                                                echo  ",'{$project_result['primkey']}' : '".htmlentities($project_result['title'])."' ";
                                            }         
                                        }
                                        echo '}"

                                });

                                jQuery(".wpbt-edit-user").editable(ajaxurl+"?action=wpbt_edit_issue", {
                                    "indicator" : "<img src=\\"'. plugins_url().'/wpbugtracktor/images/loader.gif\\">",
                                    "callback": function( sValue, y ) {
                                        uTable.fnAdjustColumnSizing();
                                        uTable.fnDraw();
                                    },
                                    "submitdata": function ( value, settings ) {
                                        return {
                                            "row_id": jQuery(this).parent().parent().attr("id"),
                                            "column": uTable.fnGetPosition( this.parentNode )[2]
                                        };
                                    },                                     
                                    "submit" : "'.__('Save', 'wpbugtracktor').'",
                                    "cancel" : "'.__('Cancel', 'wpbugtracktor').'",
                                    "type"   : "select",
                                    "data"   : " {\'0\':\''.__('Guest', 'wpbugtracktor').'\' ';
                                        global $blog_id; 
                                        $wpscBlogUsers = get_users("blog_id={$blog_id}&orderby=nicename");
                                        if(isset($wpscBlogUsers[0])) {
                                            foreach ($wpscBlogUsers as $wpscTempUser) {
                                                echo  ",'{$wpscTempUser->ID}' : '".htmlentities($wpscTempUser->display_name)."' ";
                                            }         
                                        }
                                        echo '}"

                                });

                                jQuery(".wpbt-edit-admin-user").editable(ajaxurl+"?action=wpbt_edit_issue", {
                                    "indicator" : "<img src=\\"'. plugins_url().'/wpbugtracktor/images/loader.gif\\">",
                                    "callback": function( sValue, y ) {
                                        uTable.fnAdjustColumnSizing();
                                        uTable.fnDraw();
                                    },
                                    "submitdata": function ( value, settings ) {
                                        return {
                                            "row_id": jQuery(this).parent().parent().attr("id"),
                                            "column": uTable.fnGetPosition( this.parentNode )[2]
                                        };
                                    },                                      
                                    "submit" : "'.__('Save', 'wpbugtracktor').'",
                                    "cancel" : "'.__('Cancel', 'wpbugtracktor').'",
                                    "type"   : "select",
                                    "data"   : " {\'0\':\''.__('Unassigned', 'wpbugtracktor').'\' ';
                                        global $blog_id; 
                                        $wpscBlogUsers = get_users("blog_id={$blog_id}&orderby=nicename&role={$wpBugTracktorOptions['admin_capability']}");
                                        if(isset($wpscBlogUsers[0])) {
                                            foreach ($wpscBlogUsers as $wpscTempUser) {
                                                echo  ",'{$wpscTempUser->ID}' : '".htmlentities($wpscTempUser->display_name)."' ";
                                            }         
                                        }
                                        echo '}"

                                });

                            }
                    });

                }

                jQuery(document).ready(function() {



                    wpbtInitTable();
                    
                    // Redraw any datatables
                    jQuery(window).bind("resize", function () {
                        try {
                            
                            uTable.fnAdjustColumnSizing();
                            uTable.fnDraw();

                        } catch(err) {

                        }                                            
                    } );  
                    
                    uTable.fnAdjustColumnSizing();
                    uTable.fnDraw();

                ';    
                   
            if($wpBugTracktorOptions['toggle_issue_primkey']=='true') {
                echo 'wpBugTracktorShowHide( 0, uTable );
';
            }
            if($wpBugTracktorOptions['toggle_issue_title']=='true') {
                echo 'wpBugTracktorShowHide( 1, uTable );
';                
            }
            if($wpBugTracktorOptions['toggle_issue_description']=='true') {
                 echo 'wpBugTracktorShowHide( 2, uTable );
';               
            }
            if($wpBugTracktorOptions['toggle_issue_type']=='true') {
                 echo 'wpBugTracktorShowHide( 3, uTable );
';               
            }
            if($wpBugTracktorOptions['toggle_issue_status']=='true') {
                 echo 'wpBugTracktorShowHide( 4, uTable );
';               
            }
            if($wpBugTracktorOptions['toggle_issue_reporter_id']=='true') {
                 echo 'wpBugTracktorShowHide( 5, uTable );
';               
            }
            if($wpBugTracktorOptions['toggle_issue_reporter_email']=='true') {
                echo 'wpBugTracktorShowHide( 6, uTable );
';                
            }
            if($wpBugTracktorOptions['toggle_issue_reporter_ip']=='true') {
                echo 'wpBugTracktorShowHide( 7, uTable );
';                
            }
            if($wpBugTracktorOptions['toggle_issue_owner_id']=='true') {
                echo 'wpBugTracktorShowHide( 8, uTable );
';                
            }
            if($wpBugTracktorOptions['toggle_issue_project_id']=='true') {
                echo 'wpBugTracktorShowHide( 9, uTable );
';                
            }
            if($wpBugTracktorOptions['toggle_issue_project_component_id']=='true') {
                echo 'wpBugTracktorShowHide( 10, uTable );
';                
            }
            if($wpBugTracktorOptions['toggle_issue_severity_priority']=='true') {
                echo 'wpBugTracktorShowHide( 11, uTable );
';                
            }
            if($wpBugTracktorOptions['toggle_issue_version_reported']=='true') {
                echo 'wpBugTracktorShowHide( 12, uTable );
';                
            }
            if($wpBugTracktorOptions['toggle_issue_target_fix_for_milestone_id']=='true') {
                echo 'wpBugTracktorShowHide( 13, uTable );
';                
            }
            if($wpBugTracktorOptions['toggle_issue_tags']=='true') {
                echo 'wpBugTracktorShowHide( 14, uTable );
';                
            }
            if($wpBugTracktorOptions['toggle_issue_date_reported']=='true') {
                echo 'wpBugTracktorShowHide( 15, uTable );
';                
            }
            if($wpBugTracktorOptions['toggle_issue_date_last_modified']=='true') {
                echo 'wpBugTracktorShowHide( 16, uTable );
';                
            }
            if($wpBugTracktorOptions['toggle_issue_post_id']=='true') {
                echo 'wpBugTracktorShowHide( 17, uTable );
';                
            }
            if($wpBugTracktorOptions['toggle_issue_related_issue_ids']=='true') {
                echo 'wpBugTracktorShowHide( 18, uTable );
';                
            }
            if($wpBugTracktorOptions['toggle_issue_email_updates_to_these_users']=='true') {
                echo 'wpBugTracktorShowHide( 19, uTable );
';                
            }
            if($wpBugTracktorOptions['toggle_issue_attachments']=='true') {
                echo 'wpBugTracktorShowHide( 20, uTable );
';                
            }
         
            echo ' 
                } );                
            </script>
            ' ;

            echo '<table class="widefat"><tr><td><div style="float:left;width:900px;" >
                <h2>'.__('Show these fields', 'wpbugtracktor').'</h2>
                <table>
            ';
            echo '<tr><td>'.__('Bug ID', 'wpbugtracktor'). ' <input type="checkbox" onclick="wpBugTracktorShowHide( 0, uTable );wpbtToggleViewSetting(\'toggle_issue_primkey\', this);" ';
            if($wpBugTracktorOptions['toggle_issue_primkey']=='false') {
                echo ' checked="checked" ';
            }
            echo '/></td>';
            echo '<td>'.__('Title', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 1, uTable );wpbtToggleViewSetting(\'toggle_issue_title\', this);" ';            
            if($wpBugTracktorOptions['toggle_issue_title']=='false') {
                echo ' checked="checked" ';              
            }
            echo '/></td>';
            echo '<td>'.__('Description', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 2, uTable );wpbtToggleViewSetting(\'toggle_issue_description\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_description']=='false') {
                echo ' checked="checked" ';             
            }
            echo '/></td>';
            echo '<td>'.__('Type', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 3, uTable );wpbtToggleViewSetting(\'toggle_issue_type\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_type']=='false') {
                echo ' checked="checked" ';             
            }
            echo '/></td>';
            echo '<td>'.__('Status', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 4, uTable );wpbtToggleViewSetting(\'toggle_issue_status\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_status']=='false') {
                echo ' checked="checked" ';              
            }
            echo '/></td></tr>';
            echo '<tr><td>'.__('Reported By', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 5, uTable );wpbtToggleViewSetting(\'toggle_issue_reporter_id\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_reporter_id']=='false') {
                echo ' checked="checked" ';             
            }
            echo '/></td>';
            echo '<td>'.__('Reporter Email', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 6, uTable );wpbtToggleViewSetting(\'toggle_issue_reporter_email\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_reporter_email']=='false') {
                echo ' checked="checked" ';               
            }
            echo '/></td>';
            echo '<td>'.__('Reporter IP Address', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 7, uTable );wpbtToggleViewSetting(\'toggle_issue_reporter_ip\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_reporter_ip']=='false') {
                echo ' checked="checked" ';               
            }
            echo '/></td>';
            echo '<td>'.__('Issue Assigned To', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 8, uTable );wpbtToggleViewSetting(\'toggle_issue_owner_id\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_owner_id']=='false') {
                echo ' checked="checked" ';               
            }
            echo '/></td>';
            echo '<td>'.__('Project', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 9, uTable );wpbtToggleViewSetting(\'toggle_issue_project_id\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_project_id']=='false') {
                echo ' checked="checked" ';               
            }
            echo '/></td></tr>';
            echo '<tr><td>'.__('Component', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 10, uTable );wpbtToggleViewSetting(\'toggle_issue_project_component_id\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_project_component_id']=='false') {
                echo ' checked="checked" ';              
            }
            echo '/></td>';
            echo '<td>'.__('Severity', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 11, uTable );wpbtToggleViewSetting(\'toggle_issue_severity_priority\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_severity_priority']=='false') {
                echo ' checked="checked" ';                
            }
            echo '/></td>';
            echo '<td>'.__('Version', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 12, uTable );wpbtToggleViewSetting(\'toggle_issue_version_reported\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_version_reported']=='false') {
                echo ' checked="checked" ';                
            }
            echo '/></td>';
            echo '<td>'.__('Target fix for', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 13, uTable );wpbtToggleViewSetting(\'toggle_issue_target_fix_for_milestone_id\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_target_fix_for_milestone_id']=='false') {
                echo ' checked="checked" ';               
            }
            echo '/></td>';
            echo '<td>'.__('Tags', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 14, uTable );wpbtToggleViewSetting(\'toggle_issue_tags\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_tags']=='false') {
                echo ' checked="checked" ';              
            }
            echo '/></td></tr>';
            echo '<tr><td>'.__('Date Reported', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 15, uTable );wpbtToggleViewSetting(\'toggle_issue_date_reported\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_date_reported']=='false') {
                echo ' checked="checked" ';               
            }
            echo '/></td>';
            echo '<td>'.__('Last Modified', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 16, uTable );wpbtToggleViewSetting(\'toggle_issue_date_last_modified\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_date_last_modified']=='false') {
                echo ' checked="checked" ';               
            }
            echo '/></td>';
            echo '<td>'.__('View Issue Page', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 17, uTable );wpbtToggleViewSetting(\'toggle_issue_post_id\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_post_id']=='false') {
                echo ' checked="checked" ';               
            }
            echo '/></td>';
            echo '<td style="display:none;">'.__('Related Issues', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 18, uTable );wpbtToggleViewSetting(\'toggle_issue_related_issue_ids\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_related_issue_ids']=='false') {
                echo ' checked="checked" ';             
            }
            echo '/></td>';
            echo '<td style="display:none;">'.__('Email Updates to', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 19, uTable );wpbtToggleViewSetting(\'toggle_issue_email_updates_to_these_users\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_email_updates_to_these_users']=='false') {
                echo ' checked="checked" ';            
            }
            echo '/></td></tr>';
            echo '<tr><td style="display:none;">'.__('Attachments', 'wpbugtracktor').' <input type="checkbox" onclick="wpBugTracktorShowHide( 20, uTable );wpbtToggleViewSetting(\'toggle_issue_attachments\', this);" ';              
            if($wpBugTracktorOptions['toggle_issue_attachments']=='false') {
                echo ' checked="checked" ';               
            }       
            echo '/></td><td></td><td></td><td></td><td></td></tr></table>';
            echo '</div>';
            
            echo '<table cellpadding="0" cellspacing="0" border="0" class="widefat display wpsc-data-table" id="wpBugTracktorAdminIssueTable" width="98%" >';
            echo '<thead><tr><th>'.__('Bug ID', 'wpbugtracktor').'</th><th>'.__('Title', 'wpbugtracktor').'</th><th>'.__('Description', 'wpbugtracktor').'</th><th>'.__('Type', 'wpbugtracktor').'</th><th>'.__('Status', 'wpbugtracktor').'</th><th>'.__('Reported By', 'wpbugtracktor').'</th><th>'.__('Reporter Email', 'wpbugtracktor').'</th><th>'.__('Reporter IP Address', 'wpbugtracktor').'</th><th>'.__('Issue Assigned To', 'wpbugtracktor').'</th><th>'.__('Project', 'wpbugtracktor').'</th><th>'.__('Component', 'wpbugtracktor').'</th><th>'.__('Severity', 'wpbugtracktor').'</th><th>'.__('Version', 'wpbugtracktor').'</th><th>'.__('Target fix for', 'wpbugtracktor').'</th><th>'.__('Tags', 'wpbugtracktor').'</th><th>'.__('Date Reported', 'wpbugtracktor').'</th><th>'.__('Last Modified', 'wpbugtracktor').'</th><th>'.__('View Issue Page', 'wpbugtracktor').'</th><th>'.__('Related Issues', 'wpbugtracktor').'</th><th>'.__('Email Updates to', 'wpbugtracktor').'</th><th>'.__('Attachments', 'wpbugtracktor').'</th></tr></thead><tbody>';
            echo '</tbody></table></td></tr></table>';
        }
    }
}

if(!function_exists('wpBugTracktorAdminPageSettings')) {
    function wpBugTracktorAdminPageSettings() {
        global $wpBugTracktorSettings;
        
        if(@isset($_POST['mainpage'])) { // Update options
            $wpBugTracktorSettings->setAdminOptions();
        }
        
        
        
        $wpBugTracktorOptions = get_option('wpBugTracktorAdminOptions');   
        wpBugTracktorAdminPageHeader();
        echo '
            



        <div style="padding:10px;">
            <form action="" method="post" />
                <h1>'.__('Settings', 'wpbugtracktor').'</h1>
                <table class="widefat">
                    <thead>
                        <tr><th>'.__('Configure a Main Page To Show wpBugTracktor', 'wpbugtracktor').'</th></tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td>
                                <p>'.__('This is your main wpBugTracktor parent page.  All other wpBugTracktor pages are children of this page.', 'wpbugtracktor').'  <i>'.__('This page requires the wpBugTracktor shortcode be inserted into it, like this:', 'wpbugtracktor').'</i> [wpbugtracktor]</p>
                                <select name="mainpage"> 
                                    <option value="">
                                                        ';
                                    esc_attr(__('Select page', 'wpbugtracktor'));
                                    echo '</option>'; 

                                    $pages = get_pages(); 
                                    foreach ($pages as $pagg) {
                                        $option = '<option value="'.$pagg->ID.'"';
                                        if($pagg->ID==$wpBugTracktorOptions['mainpage']) {
                                                $option .= ' selected="selected"';
                                        }
                                        $option .='>';
                                        $option .= $pagg->post_title;
                                        $option .= '</option>';
                                        echo $option;
                                    }

                                    echo '
                                </select>
                                ';

                                if(trim($wpBugTracktorOptions['mainpage'])=='') {
                                    echo ' &nbsp <span style="font-size:90%">'.__('wpBugTacktor can automatically create this page for you if you click this button: ','wpbugtracktor') .'</span> <a href="" class="button-secondary">'.__('Create My "Main Page"', 'wpbugtracktor').'</a>';
                                }    

                                echo '
                            </td>
                        </tr>
                    </tbody>
                </table>  
                <br />
                <table class="widefat">
                    <thead>
                        <tr><th>'.__('Configure Who Can Administrate wpBugTracktor', 'wpbugtracktor').'</th></tr>
                    </thead>
                    <tbody>                
                        <tr>
                            <td>
                                <p>'.__('Choose the user Roles who will have access to manage issues. ', 'wpbugtracktor').'</p>
                                <select name="admin_capability">
                                ';

                                $theOptionsAc[0] = 'administrator';$theOptionsAcName[0] = __('Administrator', 'wpbugtracktor');
                                $theOptionsAc[1] = 'editor';$theOptionsAcName[1] = __('Editor &amp; above', 'wpbugtracktor');
                                $theOptionsAc[2] = 'author';$theOptionsAcName[2] = __('Author &amp; above', 'wpbugtracktor');
                                $theOptionsAc[3] = 'contributor';$theOptionsAcName[3] = __('Contributor &amp; above', 'wpbugtracktor');
                                $fcounter=0;
                                foreach ($theOptionsAc as $theOption) {

                                        $option = '<option value="'.$theOption.'"';
                                        if($theOption == $wpBugTracktorOptions['admin_capability']) {
                                                $option .= ' selected="selected"';
                                        }
                                        $option .='>';
                                        $option .= $theOptionsAcName[$fcounter];
                                        $option .= '</option>';
                                        echo $option;
                                        $fcounter++;
                                }

                                echo '
                                </select>                            
                            </td>
                        </tr>
                    </tbody>
                </table>
                <br />
                <table class="widefat">
                    <thead>
                        <tr><th>'.__('Publishing Issues', 'wpbugtracktor').'</th></tr>
                    </thead>
                    <tbody>                
                        <tr>
                            <td>
                                <p>'.__('You should probably have a human manually review each issue before posting it live, but if you trust your users you can set it to publish immediately.', 'wpbugtracktor').'</p>
                                <select name="wpBugTracktorStatus">
                                ';

                                $theOptionsAccc[0] = 'draft';$theOptionsAcName[0] = __('Save Issues as Drafts', 'wpbugtracktor');
                                $theOptionsAccc[1] = 'publish';$theOptionsAcName[1] = __('Publish Issues immediately', 'wpbugtracktor');
                                $fcounter=0;
                                foreach ($theOptionsAccc as $theOption) {

                                        $option = '<option value="'.$theOption.'"';
                                        if($theOption == $wpBugTracktorOptions['wpBugTracktorStatus']) {
                                                $option .= ' selected="selected"';
                                        }
                                        $option .='>';
                                        $option .= $theOptionsAcName[$fcounter];
                                        $option .= '</option>';
                                        echo $option;
                                        $fcounter++;
                                }

                                echo '
                                </select>                            
                            </td>
                        </tr>
                    </tbody>
                </table>


            <br />
            <input type="submit" class="button-primary" value="'.__('Update Settings','wpbugtracktor').'" />
            </form>
        </div>
        ';
    }
}



?>