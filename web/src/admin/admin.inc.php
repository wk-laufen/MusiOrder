<?
define('ADMIN', true);
define('BASE_DIR', "..");
require_once(dirname(__FILE__) . '/../site.inc.php');
require_once(dirname(__FILE__) . '/includes/order.php');
$order = new AdminOrder();
