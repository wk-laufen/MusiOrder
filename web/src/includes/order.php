<?
class Order {
	protected $db;
	
	function __construct($db) {
		$this->db = $db;
	}
	
	function drawArticles($groups) {
		$cnt = 0;
		foreach($groups as $group) {
			$articles = $this->db->getActiveArticles($group['id']);
			$rows = 5;
			$cols = min(ceil(count($articles) / $rows), 4);
			$rows = ceil(count($articles) / $cols);
			echo '<div class="tab ui-tabs-hide" id="Tab-' . $group['id'] . '">';
			foreach($articles as $i => $article) {
				echo '<div id="Article-' . $cnt . '" class="article">
					<span class="name">' . $article['name'] . '</span>
					<a class="arrow" href="javascript:Order.countUp(' . $article['id'] . ');"><img src="images/arrow_up.png" alt="Mehr" title="Mehr" /></a>
					<span class="amount"><input id="Order-' . $article['id'] . '" type="text" value="0" name="orders[' . $article['id'] . ']" size="1" /></span>
					<a class="arrow" href="javascript:Order.countDown(' . $article['id'] . ');"><img src="images/arrow_down.png" alt="Weniger" title="Weniger" /></a>
					<div class="clear"><span>&nbsp;</span></div>
				</div>';
				$cnt++;
			}
			echo '</div>';
		}
	}
	
	function drawButtons() {
		echo '
		<input type="button" class="bigButton ui-button ui-state-default ui-corner-all" onclick="Order.showOrders();" value="Bestellungen einsehen">&nbsp;
		<input type="button" class="bigButton ui-button ui-state-default ui-corner-all" onclick="Order.resetOrder();" value="Reset">&nbsp;
		<input type="button" class="bigButton ui-button ui-state-default ui-corner-all" onclick="Order.showUserList();" value="Bestellung abschließen">';
	}
	
	function handlePostOrder($user, $articles) {
		$user = json_decode($user);
		$articles = json_decode($articles);
		if(!count($articles)) {
			echo json_encode(array('code' => 0, 'message' => 'Bestellung ist leer.'));
			exit;
		}
		
		$dbUser = $this->getUser($user->id, $user->pw);
		
		$dbArticles = $this->db->getActiveArticlesIndexedById();
		if($dbArticles === false) {
			echo json_encode(array('code' => 0, 'message' => 'Fehler beim Speichern der Bestellung.'));
			exit;
		}
		
		$this->db->startTransaction();
		foreach($articles as $article) {
			$dbArticle = $dbArticles[$article->id];
			if(!$dbArticle) {
				$this->db->cancelTransaction();
				echo json_encode(array('code' => 0, 'message' => 'Einer oder mehrere Artikel sind nicht im System vorhanden.'));
				exit;
			}
			try {
				$this->db->addOrder($dbUser['id'], $article->id, $article->amount, $dbArticle['price'], date('Y-m-d H:i:s'));
			}
			catch (\Throwable $e) {
				$this->db->cancelTransaction();
				echo json_encode(array('code' => 0, 'message' => "Fehler beim Speichern der Bestellung. $e"));
				exit;
			}
		}
		$this->db->commitTransaction();
		echo json_encode(array('code' => 1, 'message' => 'Ihre Bestellung wurde erfolgreich gespeichert.'));
	}
	
	function getOrders($userId, $timeFrom = null) {
		if($timeFrom == null) {
			$timeFrom = mktime(0, 0, 0, date('n') - 2, 1, date('Y'));
		}
		$timeTo = mktime(0, 0, 0, date('n') + 1, 1, date('Y'));
		$dbOrders = array();
		for($time = $timeFrom; $time < $timeTo; $time = mktime(0, 0, 0, date('n', $time) + 1, date('j', $time), date('Y', $time))) {
			$endTime = mktime(0, 0, 0, date('n', $time) + 1, date('j', $time), date('Y', $time));
			try {
				$dbOrders[$time] = $this->db->getOrders($userId, $time, $endTime);
			}
			catch (\Throwable $e) {
				echo json_encode(array('code' => 0, 'message' => 'Fehler beim Laden der Bestellungen.'));
				exit;
			}
		}
		// echo '<pre>';
		// print_r($dbOrders);
		try {
			$articles = $this->db->getAllArticleNamesIndexedById();
		}
		catch (\Throwable $e) {
			echo json_encode(array('code' => 0, 'message' => 'Fehler beim Laden der Bestellungen.'));
			exit;
		}
		
		$orders = array();
		foreach($dbOrders as $time => $dbArticles) {
			$orders[$time] = array();
			foreach($dbArticles as $dbOrder) {
				$order = array();
				$order['id_order_article'] = 0;
				$order['name'] = 0;
				$order['time'] = 0;
				$order['amount'] = 0;
				$order['price'] = 0;
				$order['amountSent'] = 0;
				$order['priceSent'] = 0;
				foreach($dbOrder as $dbArticle) {
					$order['id_order_article'] = $dbArticle['id_order_article'];
					$order['name'] = $articles[$order['id_order_article']];
					$order['time'] = $dbArticle['time'];
					if($dbArticle['bill_send_time'] != '') {
						$order['amountSent'] += (int)$dbArticle['amount'];
						$order['priceSent'] += (int)$dbArticle['amount'] * (float)$dbArticle['price'];
					} else {
						$order['amount'] += (int)$dbArticle['amount'];
						$order['price'] += (int)$dbArticle['amount'] * (float)$dbArticle['price'];
					}
				}
				array_push($orders[$time], $order);
			}
		}
		return $orders;
	}
	
	function handleGetOrders($user, $timeFrom = null) {
		$user = json_decode($user);

		$dbUser = $this->getUser($user->id, $user->pw);

		if($timeFrom == null) {
			$timeFrom = mktime(0, 0, 0, date('n') - 2, 1, date('Y'));
		}
		$timeTo = mktime(0, 0, 0, date('n'), 1, date('Y'));
		$orders = $this->getOrders($dbUser['idm'], $timeFrom, $timeTo);
		
		$ordersHtml = '<div style="margin: 0 10px 20px 0;">';
		for($time = $timeTo; $time >= $timeFrom; $time = mktime(0, 0, 0, date('n', $time) - 1, date('j', $time), date('Y', $time))) {
			$articles = $orders[$time];
			
			$ordersHtml .= '<div class="order">';
			$ordersHtml .= '<h2>' . strftime('%B %Y', $time) . '</h2>';
			if(count($articles)) {
				$ordersHtml .= '<table>';
				$totalPrice = 0; $totalPriceSent = 0;
				foreach($articles as $article) {
					if($article['amountSent'] > 0) {
						$ordersHtml .= '<tr>
							<td>' . date('d.m.Y', $article['time']) . ': </td>
							<td class="bill_sent">' . $article['amountSent'] . ' x ' . $article['name'] . '</td>
							<td class="bill_sent" style="text-align: right; width: 100%;">&euro; ' . number_format((float)$article['priceSent'], 2, ',', '.') . '</td>
						</tr>';
						$totalPriceSent += $article['priceSent'];
					}
					if($article['amount'] > 0) {
						$ordersHtml .= '<tr>
							<td>' . date('d.m.Y', $article['time']) . ': </td>
							<td>' . $article['amount'] . ' x ' . $article['name'] . '</td>
							<td style="text-align: right; width: 100%;">&euro; ' . number_format((float)$article['price'], 2, ',', '.') . '</td>
						</tr>';
						$totalPrice += $article['price'];
					}
				}
				$ordersHtml .= '<tr><td colspan="3"><hr /></td></tr>';
				$ordersHtml .= '<tr><td colspan="2">Gesamt:</td><td style="text-align: right;"><b>&euro; ' . sprintf("%10s", number_format($totalPrice + $totalPriceSent, 2, ',', '.')) . '</b></td></tr>';
				$ordersHtml .= '<tr><td colspan="2">Offen:</td><td style="text-align: right;"><b>&euro; ' . sprintf("%10s", number_format($totalPrice, 2, ',', '.')) . '</b></td></tr>';
				$ordersHtml .= '</table>';
			} else {
				$ordersHtml .= '<span>Keine Bestellungen vorhanden.</span>';
			}
			$ordersHtml .= '</div>';
		}
		$ordersHtml .= '</div>';
		echo json_encode(array('code' => 1, 'username' => $dbUser['nachname'] . ' ' . $dbUser['vorname'], 'message' => $ordersHtml));
	}
	
	function changeArticles($articles) {
		echo json_encode(array('code' => 0, 'message' => 'Sie haben nicht die erforderliche Berechtigung.'));
		exit;
	}
	
	function sendBill($options) {
		echo json_encode(array('code' => 0, 'message' => 'Sie haben nicht die erforderliche Berechtigung.'));
		exit;
	}
	
	function getUser($userId, $userPassword) {
		$dbUser = $this->db->getUser($userId);
		if(ADMIN) {
			return $dbUser;
		} else {
			if($userPassword != $dbUser['keyCode']) {
				echo json_encode(array('code' => 0, 'message' => 'Falsche Benutzer-Passwort-Kombination.'));
				exit;
			}
		}
		return $dbUser;
	}
}