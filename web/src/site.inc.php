<?
setlocale(LC_ALL, 'de_DE'); 

require_once(__DIR__ . '/vendor/autoload.php');
require_once(__DIR__ . '/includes/DB.php');
require_once(__DIR__ . '/includes/order.php');

if(!defined('ADMIN')) {
	define('ADMIN', false);
}

define ("ORG_NAME", "WK Laufen");

$db = new DB("db", "root", "root1234", "tennis");
$order = new Order($db);
