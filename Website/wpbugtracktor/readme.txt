=== wpBugTracktor - Bug & Issue Tracker - BETA ===
Contributors: jquindlen
Donate link: http://wpbugtracktor.com
Tags: bug,track,bug tracker,issue,bugs,issues,projects,feature request,tracker
Requires at least: 3.5.0
Tested up to: 3.9
Stable tag: 0.9.4

The best native bug tracking and issue management system for Wordpress.

== Description ==

[wpBugTracktor](http://wpbugtracktor.com/ "wpBugTracktor") is a free, open source, and full featured bug tracking and issue management system built atop of Wordpress.

**The software is fresh out the oven, and really not ready for production. This is early beta software. As in, we haven't even finished implementing all of our basic features yet. Don't worry, we're going to take care of that very quickly. Within a month or two this software will be feature complete and ready for the good old 1.0 stable production release... at least that's that goal. You can help! Give us feedback, bug reports, feature requests, and any help or encouragement you can spare. Thanks for checking out our software; we hope you enjoy it!**

It uses Wordpress pages, which keeps end users out of the dashboard area and allows you to use other comment plugins and solutions with this bug tracker.  Keep your bug tracker private, allow only certain users, all users, or even guests to report bugs.  Also optionally allows you to accept feature requests, enhancements, and ideas.

The plugin is i18n ready and compatible (POT file included in the /languages/ directory) but we ask that you hold off translation efforts until version 1.0 is complete, because there is a lot more text that will be added from now until then, which means any translations done now would be incomplete.

NOTE: This is NOT a support ticket plugin, this is bug tracking software.  However, we have also written a [free support ticket plugin](http://wpscsupporttickets.com/ "free support ticket plugin") called: wpsc Support Tickets.

== Installation ==

For complete detail and initial configuration tutorials and documentation, please visit the [Installation Documentation](http://wpstorecart.com/wpBugTracktor/ "wpBugTracktor Installation")

The recommended way to install wpBugTracktor is to go into the Wordpress admin panel, and click on Add New under the 
Plugins menu.  Search for wpBugTracktor, and then click on Install, then click Install Now.  Once the installation 
completes, Activate the plugin

Or, if you want to install manually:

1. Download the wpbugtracktor.zip file
1. Extract the zip file to your hard drive, using 7-zip or your archiver of choice.
1. Upload the `/wpbugtracktor/` directory to the `/wp-content/plugins/` directory
1. Activate the plugin through the 'Plugins' menu in WordPress
1. Create a new page, call it something like "Bug Tracker"
1. Visit the wpBugTracktor admin page and select a "mainpage" for wpBugTracktor to use, like the "Bug Tracker" page we told you to create in the last step

== Frequently Asked Questions ==

= I have questions, where do I get answers? =
- http://wpbugtracktor.com

== Screenshots ==
 
1. The optional "Setup Wizard"


== Changelog ==

= 0.9.4 =
* Fixed: Fatal error was fixed that caused new installs to not work due to the plugin's name not matching the folder names inside the source files

= 0.9.3 =
* Added: Created the /languages/ directory, added the wpbugtracktor.pot file, and added an init action hook to enable i18n support for wpbugtracktor
* Updated: The Manage Comments page got the first step towards becoming usable, but it's definitely not there yet

= 0.9.2 =
* Note: Initial Wordpress.org release
* Updated: Renamed the old issues admin page to Manage Issues
* Added: Created new Manage Comments page 

= 0.9.1 =
* Added: Added new shortcode for listing issues on a project
* Added: Added new shortcode for listing projects
* Updated: Adjusted main shortcode to show project list and issues list
* Updated: Combined all ajax calls into two files /admin/adminajax.php and /admin/publicajax.php respectively
* Updated: Removed the include calls for wp-config.php and replaced them with action hooks
* Updated: Added and corrected some strings for i7n translation

= 0.9.0 =
* Note: Initial public BETA release

== Upgrade Notice ==

= 0.9.0 =
* Initial release
