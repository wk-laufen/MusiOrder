<?
require_once(dirname(__FILE__) . '/site.inc.php');

if(!defined('BASE_DIR')) {
	define('BASE_DIR', ".");
}

$action = $_POST['do'];
if(!empty($action)) {
	switch($action) {
		case 'order':
			$user = $_POST['user'];
			$articles = $_POST['articles'];
			if(get_magic_quotes_gpc()) {
				$user = stripslashes($user);
				$articles = stripslashes($articles);
			}
			$order->handlePostOrder($user, $articles);
			break;
		case 'showOrders':
			$user = $_POST['user'];
			if(get_magic_quotes_gpc()) {
				$user = stripslashes($user);
			}
			$order->handleGetOrders($user);
			break;
		case 'changeArticles':
			$msg = $order->changeArticles($_POST['articles']);
			break;
		case 'sendBill':
			$order->sendBill($_POST['bill']);
			break;
		default:
			echo 'Wrong action';
			break;
	}
	if(!in_array($action, array('changeArticles'))) {
		exit;
	}
}
?>
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title><?php echo ORG_NAME ?> - Bestellsystem</title>
<link href="<? echo BASE_DIR; ?>/css/jquery-ui-1.7.2.custom.css" rel="stylesheet" type="text/css" />
<link href="<? echo BASE_DIR; ?>/css/order.css" rel="stylesheet" type="text/css" />
<script>
	const BASE_DIR = "<? echo BASE_DIR; ?>";
</script>
<script type="text/javascript" src="<? echo BASE_DIR; ?>/js/jquery-1.3.2.min.js"></script>
<script type="text/javascript" src="<? echo BASE_DIR; ?>/js/jquery-ui-1.7.2.custom.min.js"></script>
<script type="text/javascript" src="<? echo BASE_DIR; ?>/js/jquery.bgiframe.min.js"></script>
<script type="text/javascript" src="<? echo BASE_DIR; ?>/js/order.js"></script>
<?
if(ADMIN) {
	echo '<script type="text/javascript" src="' . BASE_DIR . '/admin/js/order.js"></script>';
	echo '<script type="text/javascript" src="' . BASE_DIR . '/admin/js/jquery.form.js"></script>';
}
if($msg) {
	echo '<script type="text/javascript">
		$(function() {
			Order.alert("Artikel speichern", "' . $msg . '");
		});
	</script>';
}
?>
</head>
<body>
<div id="Main">
	<div id="Caption">
		<div class="logo">
			<img src="<?php echo BASE_DIR ?>/images/logo.svg" height="70px" />
		</div>
		<h1>Bestellsystem</h1>
		<?php
		if(ADMIN) {
		?>
		<div class="print">
			<a href="print.php" target="_blank">Statistik drucken</a>
		</div>
		<?php
		}
		?>
	</div>
	<?
	if(ADMIN) {
		echo '<form id="FormOrder" action="' . $_SERVER['PHP_SELF'] . '" method="post">
		<div style="display: none;"><input type="hidden" name="do" value="changeArticles" /></div>';
	}
	$groups = $db->getArticleGroups();
	echo '
	<div id="Tabs" class="ui-tabs">
		<ul>';
			foreach($groups as $group) {
				echo '<li>
				  <a href="#Tab-' . $group['id'] . '">
				    <input type="hidden" name="groupId" value="' . $group['id'] . '" />
				    ' . $group['name'] . '
				  </a>
				</li>';
			}
			?>
		</ul>
		<?
		$order->drawArticles($groups);
		?>
	</div>
	<div id="Buttons">
		<? $order->drawButtons() ?>
	</div>
	<?
	if(ADMIN) {
		echo '</form>';
	}
	?>
</div>
<div id="EnterPw" title="Passwort eingeben">
	<input type="password" class="input" size="10" name="pw" />
	<table id="keyboardlower" cellspacing="7" cellpadding="0">
		<tr>
			<td class="number">
				<button id="PwSign-1" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(1);">1</button>
			</td>
			<td class="number">
				<button id="PwSign-2" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(2);">2</button>
			</td>
			<td class="number">
				<button id="PwSign-3" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(3);">3</button>
			</td>
			<td class="number">
				<button id="PwSign-4" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(4);">4</button>
			</td>
			<td class="number">
				<button id="PwSign-5" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(5);">5</button>
			</td>
			<td class="number">
				<button id="PwSign-6" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(6);">6</button>
			</td>
			<td class="number">
				<button id="PwSign-7" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(7);">7</button>
			</td>
			<td class="number">
				<button id="PwSign-8" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(8);">8</button>
			</td>
			<td class="number">
				<button id="PwSign-9" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(9);">9</button>
			</td>
			<td class="number">
				<button id="PwSign-0" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(0);">0</button>
			</td>
			<td class="number back">
				<button id="RemovePwSign" class="ui-state-default ui-corner-all" onclick="Order.removePwSign();">&larr;</button>
			</td>
		</tr>
		<tr>
			<td class="number">
				<button id="PwSign-q" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('q');">q</button>
			</td>
			<td class="number">
				<button id="PwSign-w" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('w');">w</button>
			</td>
			<td class="number">
				<button id="PwSign-e" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('e');">e</button>
			</td>
			<td class="number">
				<button id="PwSign-r" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('r');">r</button>
			</td>
			<td class="number">
				<button id="PwSign-t" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('t');">t</button>
			</td>
			<td class="number">
				<button id="PwSign-z" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('z');">z</button>
			</td>
			<td class="number">
				<button id="PwSign-u" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('u');">u</button>
			</td>
			<td class="number">
				<button id="PwSign-i" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('i');">i</button>
			</td>
			<td class="number">
				<button id="PwSign-o" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('o');">o</button>
			</td>
			<td class="number">
				<button id="PwSign-p" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('p');">p</button>
			</td>
			<td class="number back">
				<button id="PwSign-ü" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('ü');">ü</button>
			</td>
		</tr>
		<tr>
			<td class="number">
				<button id="PwSign-a" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('a');">a</button>
			</td>
			<td class="number">
				<button id="PwSign-s" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('s');">s</button>
			</td>
			<td class="number">
				<button id="PwSign-d" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('d');">d</button>
			</td>
			<td class="number">
				<button id="PwSign-f" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('f');">f</button>
			</td>
			<td class="number">
				<button id="PwSign-g" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('g');">g</button>
			</td>
			<td class="number">
				<button id="PwSign-h" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('h');">h</button>
			</td>
			<td class="number">
				<button id="PwSign-j" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('j');">j</button>
			</td>
			<td class="number">
				<button id="PwSign-k" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('k');">k</button>
			</td>
			<td class="number">
				<button id="PwSign-l" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('l');">l</button>
			</td>
			<td class="number">
				<button id="PwSign-ö" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('ö');">ö</button>
			</td>
			<td class="number back">
				<button id="PwSign-ä" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('ä');">ä</button>
			</td>
		</tr>
		<tr>
			<td class="number">
				<button id="PwSign-y" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('y');">y</button>
			</td>
			<td class="number">
				<button id="PwSign-x" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('x');">x</button>
			</td>
			<td class="number">
				<button id="PwSign-c" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('c');">c</button>
			</td>
			<td class="number">
				<button id="PwSign-v" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('v');">v</button>
			</td>
			<td class="number">
				<button id="PwSign-b" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('b');">b</button>
			</td>
			<td class="number">
				<button id="PwSign-n" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('n');">n</button>
			</td>
			<td class="number">
				<button id="PwSign-m" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('m');">m</button>
			</td>
			<td class="number">
				<button id="PwSign-," class="ui-state-default ui-corner-all" onclick="Order.addPwSign(',');">,</button>
			</td>
			<td class="number">
				<button id="PwSign-." class="ui-state-default ui-corner-all" onclick="Order.addPwSign('.');">.</button>
			</td>
			<td class="number">
				<button id="PwSign--" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('-');">-</button>
			</td>
			<td class="number back">
				<button id="PwSign-Shift" class="ui-state-default ui-corner-all" onclick="Order.keyboardToogle();">&uarr;</button>
			</td>
		</tr>
		
	</table>
	<table id="keyboardupper" cellspacing="7" cellpadding="0" style="display:none;">
		<tr>
			<td class="number">
				<button class="ui-state-default ui-corner-all" onclick="Order.addPwSign('!');">!</button>
			</td>
			<td class="number">
				<button class="ui-state-default ui-corner-all" onclick="Order.addPwSign('&quot;');">&quot;</button>
			</td>
			<td class="number">
				<button class="ui-state-default ui-corner-all" onclick="Order.addPwSign('§');">§</button>
			</td>
			<td class="number">
				<button class="ui-state-default ui-corner-all" onclick="Order.addPwSign('&#36;');">&#36;</button>
			</td>
			<td class="number">
				<button class="ui-state-default ui-corner-all" onclick="Order.addPwSign('%');">%</button>
			</td>
			<td class="number">
				<button class="ui-state-default ui-corner-all" onclick="Order.addPwSign('&amp;');">&amp;</button>
			</td>
			<td class="number">
				<button class="ui-state-default ui-corner-all" onclick="Order.addPwSign('/');">/</button>
			</td>
			<td class="number">
				<button class="ui-state-default ui-corner-all" onclick="Order.addPwSign('(');">(</button>
			</td>
			<td class="number">
				<button class="ui-state-default ui-corner-all" onclick="Order.addPwSign(')');">)</button>
			</td>
			<td class="number">
				<button class="ui-state-default ui-corner-all" onclick="Order.addPwSign('=');">=</button>
			</td>
			<td class="number back">
				<button class="ui-state-default ui-corner-all" onclick="Order.removePwSign();">&larr;</button>
			</td>
		</tr>
		<tr>
			<td class="number">
				<button id="PwSign-q" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('Q');">Q</button>
			</td>
			<td class="number">
				<button id="PwSign-w" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('W');">W</button>
			</td>
			<td class="number">
				<button id="PwSign-e" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('E');">E</button>
			</td>
			<td class="number">
				<button id="PwSign-r" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('R');">R</button>
			</td>
			<td class="number">
				<button id="PwSign-t" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('T');">T</button>
			</td>
			<td class="number">
				<button id="PwSign-z" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('Z');">Z</button>
			</td>
			<td class="number">
				<button id="PwSign-u" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('U');">U</button>
			</td>
			<td class="number">
				<button id="PwSign-i" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('I');">I</button>
			</td>
			<td class="number">
				<button id="PwSign-o" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('O');">O</button>
			</td>
			<td class="number">
				<button id="PwSign-p" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('P');">P</button>
			</td>
			<td class="number back">
				<button id="PwSign-ü" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('Ü');">Ü</button>
			</td>
		</tr>
		<tr>
			<td class="number">
				<button id="PwSign-a" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('A');">A</button>
			</td>
			<td class="number">
				<button id="PwSign-s" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('S');">S</button>
			</td>
			<td class="number">
				<button id="PwSign-d" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('D');">D</button>
			</td>
			<td class="number">
				<button id="PwSign-f" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('F');">F</button>
			</td>
			<td class="number">
				<button id="PwSign-g" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('G');">G</button>
			</td>
			<td class="number">
				<button id="PwSign-h" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('H');">H</button>
			</td>
			<td class="number">
				<button id="PwSign-j" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('J');">J</button>
			</td>
			<td class="number">
				<button id="PwSign-k" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('K');">K</button>
			</td>
			<td class="number">
				<button id="PwSign-l" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('L');">L</button>
			</td>
			<td class="number">
				<button id="PwSign-ö" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('Ö');">Ö</button>
			</td>
			<td class="number back">
				<button id="PwSign-ä" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('Ä');">Ä</button>
			</td>
		</tr>
		<tr>
			<td class="number">
				<button id="PwSign-y" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('Y');">Y</button>
			</td>
			<td class="number">
				<button id="PwSign-x" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('X');">X</button>
			</td>
			<td class="number">
				<button id="PwSign-c" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('C');">C</button>
			</td>
			<td class="number">
				<button id="PwSign-v" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('V');">V</button>
			</td>
			<td class="number">
				<button id="PwSign-b" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('B');">B</button>
			</td>
			<td class="number">
				<button id="PwSign-n" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('N');">N</button>
			</td>
			<td class="number">
				<button id="PwSign-m" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('M');">M</button>
			</td>
			<td class="number">
				<button id="PwSign-," class="ui-state-default ui-corner-all" onclick="Order.addPwSign(';');">;</button>
			</td>
			<td class="number">
				<button id="PwSign-." class="ui-state-default ui-corner-all" onclick="Order.addPwSign(':');">:</button>
			</td>
			<td class="number">
				<button id="PwSign--" class="ui-state-default ui-corner-all" onclick="Order.addPwSign('_');">_</button>
			</td>
			<td class="number back">
				<button id="PwSign-Shift" class="ui-state-default ui-corner-all" onclick="Order.keyboardToogle();">&uarr;</button>
			</td>
		</tr>
		
	</table>
<!--
		<tr>
			<td class="number">
				<button id="PwSign-7" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(7);">7</button>
			</td>
			<td class="number">
				<button id="PwSign-8" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(8);">8</button>
			</td>
			<td class="number">
				<button id="PwSign-9" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(9);">9</button>
			</td>
		</tr>
		
		<tr>
			<td class="number">
				<button id="PwSign-4" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(4);">4</button>
			</td>
			<td class="number">
				<button id="PwSign-5" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(5);">5</button>
			</td>
			<td class="number">
				<button id="PwSign-6" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(6);">6</button>
			</td>
		</tr>
		
		<tr>
			<td class="number">
				<button id="PwSign-1" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(1);">1</button>
			</td>
			<td class="number">
				<button id="PwSign-2" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(2);">2</button>
			</td>
			<td class="number">
				<button id="PwSign-3" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(3);">3</button>
			</td>
		</tr>
		
		<tr>
			<td class="number">
				<button id="PwSign-0" class="ui-state-default ui-corner-all" onclick="Order.addPwSign(0);">0</button>
			</td>
			<td class="number back" colspan="2">
				<button id="RemovePwSign" class="ui-state-default ui-corner-all" onclick="Order.removePwSign();" style="width: 65px;">&larr;</button>
			</td>
		</tr>-->
</div>
<div id="Users" title="Name auswählen">
	<input type="hidden" name="do" value="" />
	<?
	$userGroups = $db->getGroupsOfUsers();
	$groupNames = array(1 => 'Kinder', 2 => 'Jugendliche', 3 => 'Erwachsene');
	
	echo '<ul>';
	foreach($userGroups as $groupKey => $group) {
		echo '<li><a href="#Group-' . $groupKey . '">' . $groupNames[$groupKey] . '</a></li>';
	}
	echo '</ul>';
	foreach($userGroups as $groupKey => $group) {
		echo '<div id="Group-' . $groupKey . '" class="group">';
		foreach($group as $key => $user) {
			echo "<a id=\"User-" . $user['idm'] . "\" class=\"user\" href=\"javascript:Order.selectUser(" . $user['idm'] . ");\">" . trim($user['nachname']) . " " . trim($user['vorname']) . "</a>";
		}
		echo '</div>';
	}
	?>
</div>
<div id="Orders" title=""></div>
<?
if(ADMIN) {
	echo '<div id="SendBill" title="Rechnung versenden">
		<form method="post" action="' . $_SERVER['PHP_SELF'] . '">
			<div style="display: none;"><input type="hidden" name="do" value="sendBill" /></div>
			<div class="option">
				<span>Bestelltes bis inkl. <select name="bill[timeTo]">';
					echo '<option value="' . mktime(23, 59, 59, date('n'), date('j'), date('Y')) . '"' . '>Heute</option>';
					for($i = date('n') - 1; $i > date('n') - 12; $i--) {
						echo '<option value="' . mktime(23, 59, 59, $i + 1, 0, date('Y')) . '"' . '>' . strftime('%B %Y', mktime(0, 0, 0, $i, 1, date('Y'))) . '</option>';
					}
					echo '
				</select> versenden.</span>
			</div>
			<div>
				<span>An:<br />
				<input id="SendBillToAll" type="radio" name="bill[recipients]" value="all" checked="checked" /> Alle Mitglieder<br />
				<input id="SendBillToSeveral" type="radio" name="bill[recipients]" value="several" /> <a href="javascript:Order.chooseUsersForBill();">Ausgewählte Mitglieder (<span id="cntSelectedUser">0</span>)</a>
				<input type="hidden" name="bill[recipientIds]" value="" />
				</span>
			</div>
			<!--
			<div>
				<span>Mindestrechnungsbetrag:
				<input id="SendBillToAll" type="text" name="bill[priceFloor]" size="3" /> &euro;
				</span>
			</div>
			-->
			<div>
				<span>
					<input type="checkbox" name="bill[sendCopy]" />
					Kopie aller Rechnungen senden an
					<input type="text" name="bill[copyAddress]" />
				</span>
			</div>
		</form>
	</div>';
}
?>
</body>
</html>
