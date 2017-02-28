<?php
/** 
 * The base configurations of the WordPress.
 *
 * This file has the following configurations: MySQL settings, Table Prefix,
 * Secret Keys, WordPress Language, and ABSPATH. You can find more information by
 * visiting {@link http://codex.wordpress.org/Editing_wp-config.php Editing
 * wp-config.php} Codex page. You can get the MySQL settings from your web host.
 *
 * This file is used by the wp-config.php creation script during the
 * installation. You don't have to use the web site, you can just copy this file
 * to "wp-config.php" and fill in the values.
 *
 * @package WordPress
 */

// ** MySQL settings - You can get this info from your web host ** //
/** The name of the database for WordPress */
define('DB_NAME', 'wordpress_4');

/** MySQL database username */
define('DB_USER', 'wordpress_b');

/** MySQL database password */
define('DB_PASSWORD', '29lGYaA$7w');

/** MySQL hostname */
define('DB_HOST', 'localhost:3306');

/** Database Charset to use in creating database tables. */
define('DB_CHARSET', 'utf8');

/** The Database Collate type. Don't change this if in doubt. */
define('DB_COLLATE', '');

/**#@+
 * Authentication Unique Keys.
 *
 * Change these to different unique phrases!
 * You can generate these using the {@link http://api.wordpress.org/secret-key/1.1/ WordPress.org secret-key service}
 *
 * @since 2.6.0
 */
define('AUTH_KEY',         'X6!8W9F16bmakxCu$B6%ZpHFugUW5iqpZSiQI&P*mfm5S9RJC6EA^Lqb%FF4Y!3R');
define('SECURE_AUTH_KEY',  'zxPsOqbjihDSx2tsZT$VBIR%Rm$JY3J0Ec*u23WG6W*z^d)7SjgGJdwkpbLZy7Ou');
define('LOGGED_IN_KEY',    '$r6LZ$oXaJ7$dK7!z^@CqnSgzbQbNKJt3(3I9qlBjPxMes*#W6Wq2%ltWa4B)EU5');
define('NONCE_KEY',        'Ug%uC*Hi6$jZ&8)dGGvj$dmOd3ksf$!1bADcm61SZnqTmtPyZu4du&TTKl2zW^UV');
define('AUTH_SALT',        '$&gi!z48Y3^@)%XxCVfGB)PH7Dj(uvJ#XhSgPv$yIHEt40R$X!Wry!Ktu$iPGjzH');
define('SECURE_AUTH_SALT', '!Jeuusd(bZ4Tl%wteCz3yhhMgrcLNJ4LobDSgKzjVS#14T%ZlfXTg@eJ)uD$LWR!');
define('LOGGED_IN_SALT',   'kahrxN34Mm%C8Foa74Ok7)u(F!!NlzjKR)TGzkDb*ak1(054L*4E6o&@)*argbNf');
define('NONCE_SALT',       'Hh$gc9PDnL5USSlMv3@5zXzEkMueKry^JHHxZeK3BBaUVhcU5SK7Oa65S3H3^fR4');
/**#@-*/

/**
 * WordPress Database Table prefix.
 *
 * You can have multiple installations in one database if you give each a unique
 * prefix. Only numbers, letters, and underscores please!
 */
$table_prefix  = 'wp_';

/**
 * WordPress Localized Language, defaults to English.
 *
 * Change this to localize WordPress.  A corresponding MO file for the chosen
 * language must be installed to wp-content/languages. For example, install
 * de.mo to wp-content/languages and set WPLANG to 'de' to enable German
 * language support.
 */
define ('WPLANG', 'en_US');

define ('FS_METHOD', 'direct');

define('WP_DEBUG', false);

/* That's all, stop editing! Happy blogging. */

/** WordPress absolute path to the Wordpress directory. */
if ( !defined('ABSPATH') )
	define('ABSPATH', dirname(__FILE__) . '/');

/** Sets up WordPress vars and included files. */
require_once(ABSPATH . 'wp-settings.php');

//--- disable auto upgrade
define( 'AUTOMATIC_UPDATER_DISABLED', true );



?>
