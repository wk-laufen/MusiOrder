<?
require_once(dirname(__FILE__).'/includes/functions.inc.php');
require_once('MDB2.php');
require_once(dirname(__FILE__).'/includes/order.php');

setlocale(LC_ALL, 'de_DE'); 

$dsn = 'mysql://user:password@localhost/tennis';
$db =& MDB2::factory($dsn);
if (PEAR::isError($db)) {
    echo ($db->getMessage().' - '.$db->getUserinfo());
}
$db->setFetchMode(MDB2_FETCHMODE_ASSOC);

if(!defined('ADMIN')) {
	define('ADMIN', false);
}

define('DB_TABLE_ORDERS', 'orders');
define('DB_TABLE_ORDER_ARTICLES', 'order_articles');
define('DB_TABLE_ORDER_ARTICLE_GROUPS', 'order_article_groups');

$order =& new Order();
