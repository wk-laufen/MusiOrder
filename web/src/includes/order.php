<?
require_once(dirname(__FILE__).'/JSON.php');
require_once(dirname(__FILE__).'/../../includes/password.inc');
class Order {
	var $db, $json;
	
	function Order() {
		$this->db = $GLOBALS['db'];
		$this->json = new Services_JSON(SERVICES_JSON_LOOSE_TYPE);
	}
	
	function drawArticles($groups) {
	  $cnt = 0;
		foreach($groups as $group) {
			$articles = $this->getArticles($group['id']);
			$rows = 5;
	    $cols = min(ceil(count($articles) / $rows), 4);
	    $rows = ceil(count($articles) / $cols);
			echo '<div class="tab ui-tabs-hide" id="Tab-' . $group['id'] . '">
				<div>';
				echo '<table><tr><td>';
				foreach($articles as $i => $article) {
				  if($i && !($i % $rows)) {
				   echo '</td><td>'; 
				  }
    	    echo '<div id="Article-' . $cnt . '" class="article">
      			<span class="name">' . $article['name'] . '</span>
      			<a class="arrow" href="javascript:Order.countUp(' . $article['id'] . ');"><img src="images/arrow_up.png" alt="Mehr" title="Mehr" /></a>
      			<span class="amount"><input id="Order-' . $article['id'] . '" type="text" value="0" name="orders[' . $article['id'] . ']" size="1" /></span>
      			<a class="arrow" href="javascript:Order.countDown(' . $article['id'] . ');"><img src="images/arrow_down.png" alt="Weniger" title="Weniger" /></a>
      			<div class="clear"><span>&nbsp;</span></div>
      		</div>';
      		$cnt++;
    		}
    		echo '</td></tr></table></div></div>';
		}
	}
	
	function drawButtons() {
		echo '
		<input type="button" class="bigButton ui-button ui-state-default ui-corner-all" onclick="Order.showOrders();" value="Bestellungen einsehen">&nbsp;
		<input type="button" class="bigButton ui-button ui-state-default ui-corner-all" onclick="Order.resetOrder();" value="Reset">&nbsp;
		<input type="button" class="bigButton ui-button ui-state-default ui-corner-all" onclick="Order.showUserList();" value="Bestellung abschließen">';
	}
	
	function getArticles($groupId) {
		return $this->db->queryAll('SELECT * FROM ' . DB_TABLE_ORDER_ARTICLES . ' WHERE id_group=' . $this->db->escape($groupId) . ' AND state = 1 ORDER BY grade');
	}
	
	function doOrder($user, $articles) {
		if(!count($articles)) {
			echo $this->json->encode(array('code' => 0, 'message' => 'Bestellung ist leer.'));
			exit;
		}
		
		$this->checkUser($user);
		
		$dbArticles = $this->db->queryAll('SELECT id, name, price FROM ' . DB_TABLE_ORDER_ARTICLES, null, null, true);
		if(PEAR::isError($dbArticles)) {
			echo $this->json->encode(array('code' => 0, 'message' => 'Fehler beim lesen Speichern der Bestellung.'));
			exit;
		}
		
		$this->db->query('START TRANSACTION');
		foreach($articles as $article) {
			$dbArticle = $dbArticles[$article['id']];
			if(!$dbArticle) {
				$this->db->query('ROLLBACK');
				echo $this->json->encode(array('code' => 0, 'message' => 'Einer oder mehrere Artikel sind nicht im System vorhanden.'));
				exit;
			}
			$query = 'INSERT INTO ' . DB_TABLE_ORDERS . ' SET ';
			$query .= 'id_user = ' . $this->db->escape($user['id']);
			$query .= ', id_order_article = ' . $this->db->escape($article['id']);
			$query .= ', amount = ' . $this->db->escape($article['amount']);
			$query .= ', price = ' . number_format((float)$this->db->escape($dbArticle['price']), 2, '.', ',');
			$query .= ', `time` = "' . time() . '"';
			$query .= ', ip = "' . $this->db->escape($_SERVER['REMOTE_ADDR']) . '"';
			$res = $this->db->query($query);
			if(PEAR::isError($res)) {
				$this->db->query('ROLLBACK');
				echo $this->json->encode(array('code' => 0, 'message' => 'Fehler beim Speichern der Bestellung.'.$query));
				exit;
			}
		}
		$this->db->query('COMMIT');
		echo $this->json->encode(array('code' => 1, 'message' => 'Ihre Bestellung wurde erfolgreich gespeichert.'));
	}
	
	function getOrders($user, $timeFrom = null) {
		if($timeFrom == null) {
			$timeFrom = mktime(0, 0, 0, date('n') - 2, 1, date('Y'));
		}
		$timeTo = mktime(0, 0, 0, date('n') + 1, 1, date('Y'));
		$dbOrders = array();
		for($time = $timeFrom; $time < $timeTo; $time = mktime(0, 0, 0, date('n', $time) + 1, date('j', $time), date('Y', $time))) {
			$dbOrders[$time] = $this->db->queryAll('SELECT CONCAT(id_order_article, ";", DATE_FORMAT(FROM_UNIXTIME(`time`), "%d.%m.%Y")) as thekey, id_order_article, amount, price, time, bill_send_time  FROM ' . DB_TABLE_ORDERS . ' WHERE `time` >= ' . $time . ' AND `time` < ' . mktime(0, 0, 0, date('n', $time) + 1, date('j', $time), date('Y', $time)) . ($user['id'] > 0? ' AND id_user = ' . $this->db->escape($user['id']): '') . ' ORDER BY `time`', null, null, true, null, true);
			if(PEAR::isError($dbOrders[$time])) {
				echo $this->json->encode(array('code' => 0, 'message' => 'Fehler beim Laden der Bestellungen.'));
				exit;
			}
		}
		//echo '<pre>';
		//print_r($dbOrders);
		$articles = $this->db->queryAll('SELECT id, name FROM ' . DB_TABLE_ORDER_ARTICLES, null, null, true);
		if(PEAR::isError($articles)) {
			echo $this->json->encode(array('code' => 0, 'message' => 'Fehler beim Laden der Bestellungen.'));
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
	
	function showOrders($user, $timeFrom = null) {
		$this->checkUser($user);
		
		if($timeFrom == null) {
			$timeFrom = mktime(0, 0, 0, date('n') - 2, 1, date('Y'));
		}
		$timeTo = mktime(0, 0, 0, date('n'), 1, date('Y'));
		$orders = $this->getOrders($user, $timeFrom, $timeTo);
		
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
		return array('code' => 1, 'username' => $user['nachname'] . ' ' . $user['vorname'], 'message' => utf8_encode($ordersHtml));
	}
	
	function changeArticles($articles) {
		echo $this->json->encode(array('code' => 0, 'message' => 'Sie haben nicht die erforderliche Berechtigung.'));
		exit;
	}
	
	function sendBill($options) {
		echo $this->json->encode(array('code' => 0, 'message' => 'Sie haben nicht die erforderliche Berechtigung.'));
		exit;
	}
	
	function checkUser($user) {
		// super-pw
		/*if($user['pw'] == 'Buchung123') {
			$user = $this->db->queryRow('SELECT * FROM mitglieder WHERE idm = ' . $this->db->escape($user['id']));
		} else {
			$user = $this->db->queryRow('SELECT * FROM mitglieder WHERE idm = ' . $this->db->escape($user['id']) . ' AND tennis_pw = "' . $this->db->escape($user['pw']) . '"');
		}*/
		if(ADMIN) {
			
		} else {
			$userpass = $this->db->queryRow('SELECT pass FROM mitglieder WHERE IDM="'.$this->db->escape($user['id']).'"');
			if(!$this->checkdrupal_password($user['pw'], $userpass['pass'])) {
				echo $this->json->encode(array('code' => 0, 'message' => 'Falsche Benutzer-Passwort-Kombination.'));
				exit;
			}
		}
	}
/*
	function verifyPassword() {
	$password = $_POST['user_pw'];
		$userpass = $this->db->queryRow('SELECT pass FROM mitglieder WHERE IDM="'.$_POST['user_id'].'"');
		if($this->checkdrupal_password($password,$userpass['pass']))
			$ret = array('code'=>'1','message'=>'ok');
		else
			$ret = array('code'=>'0','message'=>'Falsches Passwort!: '.$userpass['pass']);
		
		echo $this->_encode($ret);
	}

*/
function checkdrupal_password($password, $userpass) {
  if (substr($userpass, 0, 2) == 'U$') {
    // This may be an updated password from user_update_7000(). Such hashes
    // have 'U' added as the first character and need an extra md5().
    $stored_hash = substr($userpass, 1);
    $password = md5($password);
  }
  else {
    $stored_hash = $userpass;
  }
  $type = substr($stored_hash, 0, 3);
  switch ($type) {
    case '$S$':
      // A normal Drupal 7 password using sha512.
      $hash = _password_crypt('sha512', $password, $stored_hash);
      break;
    case '$H$':
      // phpBB3 uses "$H$" for the same thing as "$P$".
    case '$P$':
      // A phpass password generated using md5.  This is an
      // imported password or from an earlier Drupal version.
      $hash = _password_crypt('md5', $password, $stored_hash);
      break;
    default:
      return FALSE;
  }
  return ($hash && $stored_hash == $hash);
}
}
