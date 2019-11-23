<?
require_once(dirname(__FILE__) . '/../../includes/' . basename(__FILE__));
class AdminOrder extends Order {
	function drawArticles($groups) {
	  $cnt = 0;
	  foreach($groups as $group) {
			$articles = $this->getArticles($group['id']);
			echo '<div class="tab ui-tabs-hide" id="Tab-' . $group['id'] . '"><div class="sortable">';
		  if(!count($articles)) {
				$articles = array(array('id_group' => $group['id']));
			}
			foreach($articles as $i => $article) {
    		echo '<div id="Article-' . $cnt . '" class="article">
    			<input type="hidden" name="articles[' . $cnt . '][id]" value="' . $article['id'] . '" />
    			<input type="hidden" name="articles[' . $cnt . '][id_group]" value="' . $article['id_group'] . '" />
    			<input type="hidden" name="articles[' . $cnt . '][state]" value="' . $article['state'] . '" />
    			<input type="hidden" name="articles[' . $cnt . '][trash]" value="0" />
    			<input type="hidden" name="articles[' . $cnt . '][grade]" value="' . ($i + 1) . '" />
    			<span class="name">
    				<input type="text" value="' . $article['name'] . '" name="articles[' . $cnt . '][name]" />
    			</span>
    			<span class="price">Preis: <input type="text" value="' . number_format((float)$this->db->escape($article['price']), 2, ',', '.') . '" name="articles[' . $cnt . '][price]" size="4" /> &euro;</span>
    			<span class="grade"><img src="/order/images/move.png" alt="Sortieren" title="Sortieren" /></span>
    			<a class="state" href="javascript:Order.toggleState(' . $cnt . ');"><img src="/order/images/state_' . (int)$article['state'] . '.png" alt="' . ((int)$article['state']?'Online':'Offline') . '" title="' . ((int)$article['state']?'Online':'Offline') . '" /></a>
    			<a class="trash" href="javascript:Order.toggleTrash(' . $cnt . ');"><img src="/order/images/trash_0.png" alt="Artikel nicht löschen" title="Artikel nicht löschen" /></a>
    			<div class="clear"><span>&nbsp;</span></div>
    		</div>';
    		$cnt++;
    	}
      echo '</div>
      <div class="options">
  		  <input type="button" class="bigButton ui-button ui-state-default ui-corner-all" onclick="Order.addArticle(' . $group['id'] . ');" value="Artikel hinzufügen" />&nbsp;
  	  </div></div>';
		}
	}
	
	function drawButtons() {
		echo '
		<input type="submit" class="bigButton ui-button ui-state-default ui-corner-all" value="Änderungen speichern" />&nbsp;
		<input type="button" class="bigButton ui-button ui-state-default ui-corner-all" onclick="Order.showOrders();" value="Bestellungen einsehen" />&nbsp;
		<input type="button" class="bigButton ui-button ui-state-default ui-corner-all" onclick="Order.sendBill();" value="Rechnung versenden" />';
	}
	
	function getArticles($groupId) {
		return $this->db->queryAll('SELECT * FROM ' . DB_TABLE_ORDER_ARTICLES . ' WHERE id_group=' . $this->db->escape($groupId) . ' AND state >= 0 ORDER BY grade');
	}
	
	function changeArticles($articles) {
		$msg = '';
		foreach($articles as $article) {
			if(get_magic_quotes_gpc()) {
				$article['name'] = stripslashes($article['name']);
			}
			$sql = 'REPLACE ' . DB_TABLE_ORDER_ARTICLES . ' SET ';
			if($article['id']) {
				$sql .= 'id = ' . $this->db->escape($article['id']) . ', ';
			} elseif(!$article['name']) {
				continue;
			}
			
			$pos1 = strpos($article['price'], '.');
			$pos2 = strpos($article['price'], ',');
			if($pos1 !== false && $pos2 !== false) {
				if($pos1 < $pos2) {
					$article['price'] = str_replace('.', '', $article['price']);
				} else {
					$article['price'] = str_replace(',', '', $article['price']);
				}
			}
			$article['price'] = str_replace(',', '.', $article['price']);
			
			if($article['trash']) {
				$article['state'] = -1;
			}
			
			$sql .= 'id_group = ' . $this->db->escape((int)$article['id_group']) .
				', state = ' . $this->db->escape((int)$article['state']) .
				', grade = ' . $this->db->escape((int)$article['grade']) .
				', name = "' . $this->db->escape($article['name']) . '"' .
				', price = ' . $this->db->escape($article['price']);
			
			$ret = $this->db->query($sql);
			if(PEAR::isError($ret)) {
				$msg = 'Fehler beim Speichern eines oder mehrerer Artikel.';
			}
		}
		
		if(!$msg) {
			$msg = 'Artikel erfolgreich gespeichert.';
		}
		return $msg;
	}
	
	function sendBill($options) {
		require_once(dirname(__FILE__).'/phpMailer/class.phpmailer.php');
		
		$mail = new PHPMailer();
		$mail->SetLanguage('de', dirname(__FILE__) . '/phpMailer/language/');
		$mail->From       = 'no-reply@union-gampern.at';
		$mail->FromName   = 'Sportunion Gampern';
		
		$timeTo = $options['timeTo'];
		
		if($options['recipients'] == 'all') {
			$users = $this->db->queryAll('SELECT idm, CONCAT_WS(" ", nachname, vorname) as full_name, email FROM mitglieder');
		} else {
			$users = $this->db->queryAll('SELECT idm, CONCAT_WS(" ", nachname, vorname) as full_name, email FROM mitglieder WHERE idm IN (' . $options['recipientIds'] . ')');
		}
		if(!count($users) || PEAR::isError($users)) {
			echo $this->json->encode(array('code' => 0, 'message' => 'Fehler beim Senden der Rechnungen: Datenbank-Fehler'));
			exit;
		}
		
		$articles = $this->db->queryAll('SELECT id, name FROM ' . DB_TABLE_ORDER_ARTICLES, null, null, true);
		if(PEAR::isError($articles)) {
			echo $this->json->encode(array('code' => 0, 'message' => 'Fehler beim Senden der Rechnungen: Datenbank-Fehler'));
			exit;
		}
		
		$sendCopy = ((bool)$options['sendCopy'] && (bool)$options['copyAddress']);
		if($sendCopy) {
			$copyAddress = $options['copyAddress'];
		}
		
		$userStat = array(
			'haveOrders' => array(),
			'tooLessOrders' => array(),
			'noEmail' => array(),
			'errors' => array()
		);
		
		foreach($users as $key => $user) {
			$users[$key]['orders'] = $this->db->queryAll('SELECT id_order_article, id, amount, price  FROM ' . DB_TABLE_ORDERS . ' WHERE `time` <= ' . $timeTo . ' AND bill_send_time = "" AND id_user = ' . $this->db->escape($user['idm']), null, null, true, null, true);
			if(PEAR::isError($users[$key]['orders'])) {
				echo $this->json->encode(array('code' => 0, 'message' => 'Fehler beim Senden der Rechnungen: Datenbank-Fehler'));
				exit;
			}
			if(!$users[$key]['orders']) {
				unset($users[$key]);
			} else {
				array_push($userStat['haveOrders'], $users[$key]['full_name']);
			}
		}
		
		$sendTime = time();
		foreach($users as $user) {
			if(!trim($user['email'])) {
				array_push($userStat['noEmail'], $user['full_name']);
				continue;
			}
			$mailHtml = "Liebes Clubmitglied,\n\nnachstehend übermitteln wir dir eine Auflistung der konsumierten Einheiten.\nWir bitten dich den fälligen Betrag innerhalb von 2 Wochen auf folgendes Konto zu überweisen.\n\nUnion Gampern/Sekt.Tennis\nIBAN: AT31 3411 3000 0002 0388\n\nSollte der betreffende Betrag nicht innerhalb des Zahlungszieles\nüberwiesen werden,so müssen wir ihnen Bearbeitungsgebühren von 3€ verrechnen.\n\nmit freundlichen Grüßen\nEuer Kassier\n\n";
			$totalPrice = 0;
			foreach($user['orders'] as $articleId => $article) {
				$articleAmount = 0;
				$articlePrice = 0;
				foreach($article as $order) {
					$articleAmount += (int)$order['amount'];
					$articlePrice += (int)$order['amount'] * (float)$order['price'];
				}
				$totalPrice += $articlePrice;
				$mailHtml .= sprintf('%3s x %-30s &euro; %6s', $articleAmount, $articles[$articleId], number_format($articlePrice, 2, ',', '.')) . "\n";
			}
			
			if(false && $totalPrice < 20) {
				array_push($userStat['tooLessOrders'], $user['full_name']);
				continue;
			}
			$mailHtml .= str_repeat('-', 47) . "\n" . sprintf('%36s &euro; %6s', 'Gesamt:', number_format($totalPrice, 2, ',', '.'));
			
			$mail->Subject = date('m/Y', $timeTo) . " Rechnung für " . $user['full_name'];
			$mail->AltBody = $mailHtml;
			$mail->MsgHTML('<pre>' . $mailHtml . '</pre>');
			$mail->AddAddress($user['email'], $user['full_name']);
			//$mail->AddAddress('krr@sml.at', 'Karrer Hans-Peter');
			//$mail->AddAddress('egger_j@gmx.net', 'Egger Johannes');
			if($sendCopy) {
				$mail->AddBCC($copyAddress);
			}
			
			$this->db->query('START TRANSACTION');
			foreach($user['orders'] as $article) {
				foreach($article as $order) {
					$res = $this->db->query('UPDATE ' . DB_TABLE_ORDERS . ' SET bill_send_time = "' . $sendTime . '" WHERE id = ' . $this->db->escape($order['id']));
					if(PEAR::isError($res)) {
						$this->db->query('ROLLBACK');
						array_push($userStat['errors'], $user['full_name']);
						continue 3;
					}
				}
			}
			
			if(!$mail->Send()) {
				$this->db->query('ROLLBACK');
				array_push($userStat['errors'], $user['full_name']);
				continue;
			}
			$this->db->query('COMMIT');
			$mail->ClearAllRecipients();
		}
		$error = !(bool)(count($userStat['errors']));
		$message = count($userStat['haveOrders']) . ' Mitglied(er) mit Bestellungen im angegeben Zeitraum gefunden.';
		//$message .= '<br />' . count($userStat['tooLessOrders']) . ' Mitglied(er) unterschritt(en) den Bestellwert von 20&euro;.';
		$message .= '<br />' . count($userStat['noEmail']) . ' Mitglied(er) konnte aufgrund einer fehlenden E-Mail-Adresse keine Rechnung gesendet werden.';
		$message .= '<br />Bei ' . count($userStat['errors']) . ' Mitglied(ern) trat(en) w&auml;hrend dem Sendevorgang ein Fehler auf.';
		echo $this->json->encode(array('code' => (int)$error, 'message' => $message));
	}
}
