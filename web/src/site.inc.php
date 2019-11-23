<?
setlocale(LC_ALL, 'de_DE'); 

require_once(dirname(__FILE__).'/includes/functions.inc.php');
require_once(dirname(__FILE__).'/includes/DB.php');
require_once(dirname(__FILE__).'/includes/order.php');

$db = new DB("db", "root", "root1234", "tennis");

if(!defined('ADMIN')) {
	define('ADMIN', false);
}

define('DB_TABLE_ORDERS', 'orders');
define('DB_TABLE_ORDER_ARTICLES', 'order_articles');
define('DB_TABLE_ORDER_ARTICLE_GROUPS', 'order_article_groups');

$order = new Order();
