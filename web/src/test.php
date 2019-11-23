<?php
$a = array();
if ($a) { echo "Empty array is true-ish."; }
else { echo "Empty array is false-ish."; }

if ($a === false) { echo "Empty array is false."; }
else { echo "Empty array is not false."; }

require_once "site.inc.php";
$articles = $db->queryAllIndexByCol('SELECT id, name, price FROM ' . DB_TABLE_ORDER_ARTICLES, "id");
print_r($articles);
?>