<?php
require_once(dirname(__FILE__) . '/admin.inc.php');
$year = $_GET['year'];
if(!(int)$year) { $year = date('Y'); }
$timeFrom = mktime(0, 0, 0, 1, 1, $year);
$timeTo = mktime(0, 0, 0, 12, 1, $year);
$user = $_GET['user'];
if($user['id']) {
	$dbUser = $order->getUser($user['id'], "");
	$fullName = $dbUser['nachname'] . ' ' . $dbUser['vorname'];
} else {
	$fullName = 'alle Mitglieder';
}
$orders = $order->getOrders($user['id'], $timeFrom);
?>
<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
<title><?php echo ORG_NAME ?> - Bestellsystem - Bestellungen für <?php echo $fullName; ?> drucken</title>
<link rel="shortcut icon" type="image/x-icon" href="<?php echo BASE_DIR ?>/images/logo.ico" sizes="256x256">
<style type="text/css">
	#Orders {
		width: 600px;
	}
	#Orders table {
		width: 100%;
	}
	#Orders td.order {
		vertical-align: top;
	}
	#Orders .order table td {
		white-space: nowrap;
	}
	#Orders .order table td.bill_sent {
		text-decoration: line-through;
	}
	#Orders .order table td.price {
		text-align: right;
		width: 100%;
	}
	.screen-only {
		display: block;
	}
	.print-only {
		display: none;
	}
</style>
<style type="text/css" media="print">
	.screen-only {
		display: none;
	}
	.print-only {
		display: block;
	}
</style>
</head>
<body>
	<div class="screen-only">
		<form action="<?php echo $_SERVER['PHP_SELF']; ?>">
			<input type="hidden" name="user[id]" value="<?php echo $_GET['user']['id']; ?>" />
			<div><span>Jahr:</span>
				<select name="year">
					<?php
					for($i = date('Y'); $i > date('Y') - 5; $i--) {
						echo '<option value="' . $i . '"' . ($year == $i? 'selected="selected"': '') . '>' . $i . '</option>';
					}
					?>
				</select>
				<input type="submit" value="Bestellungen ansehen" />
			</div>
		</form>
	</div>
	<h1><?php echo 'Jahresstatistik ' . $year . ' für ' . $fullName; ?></h1>
	<div id="Orders">
		<?php
			$col = 0;
			echo '<table width="100%"><tr>';
			for($time = $timeTo; $time >= $timeFrom; $time = mktime(0, 0, 0, date('n', $time) - 1, date('j', $time), date('Y', $time))) {
				if($time > time()) {
					continue;
				}
				$articles = $orders[$time];
				if($col++ == 1) {
					$col = 1;
					echo '</tr><tr>';
				}
				echo '<td class="order">';
				echo '<h2>' . strftime('%B %Y', $time) . '</h2>';
				if(count($articles)) {
					$totalPrice = 0; $totalPriceSent = 0;
					echo '<table>';
					$lastTime = '';
					foreach($articles as $article) {
						$timeStr = strftime('%a, %d.%m', $article['time']);
						$displayTime = ($lastTime != $timeStr);
						$lastTime = $timeStr;
						if($article['amountSent'] > 0) {
							echo '<tr>
								<td>' . ($displayTime? $timeStr . ': ': '&nbsp;') . '</td>
								<td class="bill_sent">' . $article['amountSent'] . ' x ' . $article['name'] . '</td>
								<td class="bill_sent price">&euro; ' . number_format((float)$article['priceSent'], 2, ',', '.') . '</td>
							</tr>';
							$totalPriceSent += $article['priceSent'];
							$displayTime = false;
						}
						if($article['amount'] > 0) {
							echo '<tr>
								<td>' . ($displayTime? $timeStr . ': ': '&nbsp;') . '</td>
								<td>' . $article['amount'] . ' x ' . $article['name'] . '</td>
								<td class="price">&euro; ' . number_format((float)$article['price'], 2, ',', '.') . '</td>
							</tr>';
							$totalPrice += $article['price'];
						}
					}
					echo '<tr><td colspan="3"><hr /></td></tr>';
					echo '<tr><td colspan="2">Gesamt:</td><td style="text-align: right;"><b>&euro; ' . sprintf("%10s", number_format($totalPrice + $totalPriceSent, 2, ',', '.')) . '</b></td></tr>';
					echo '<tr><td colspan="2">Offen:</td><td style="text-align: right;"><b>&euro; ' . sprintf("%10s", number_format($totalPrice, 2, ',', '.')) . '</b></td></tr>';
					echo '</table>';
				} else {
					echo '<span>Keine Bestellungen vorhanden.</span>';
				}
			}
			echo '</tr></table>';
		?>
	</div>
</body>
</html>
