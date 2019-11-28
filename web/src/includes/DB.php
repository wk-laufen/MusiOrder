<?php

class DB {
    private $db;

    function DB($host, $username, $password, $dbName) {
        try {
            $this->db = new PDO("mysql:host=$host;dbname=$dbName;charset=utf8", $username, $password);
            $this->db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
        } catch (PDOException $e) {
            die("Failed to connect to database: {$e->getMessage()}");
        }
    }

    private function queryOne($statement, $params) {
        $preparedStatement = $this->db->prepare($statement);
        $preparedStatement->execute($params);
        return $preparedStatement->fetch(PDO::FETCH_ASSOC);
    }

    private function queryAll($statement, $params) {
        $preparedStatement = $this->db->prepare($statement);
        $preparedStatement->execute($params);
        return $preparedStatement->fetchAll(PDO::FETCH_ASSOC);
    }

    private function queryAllGroupedByFirstColumn($statement, $params) {
        $preparedStatement = $this->db->prepare($statement);
        $preparedStatement->execute($params);
        return $preparedStatement->fetchAll(PDO::FETCH_GROUP|PDO::FETCH_ASSOC);
    }

    private function queryAllIndexedByFirstColumn($statement, $params) {
        $preparedStatement = $this->db->prepare($statement);
        $preparedStatement->execute($params);
        return $preparedStatement->fetchAll(PDO::FETCH_UNIQUE|PDO::FETCH_ASSOC);
    }

    private function queryAllKeyValue($statement, $params) {
        $preparedStatement = $this->db->prepare($statement);
        $preparedStatement->execute($params);
        return $preparedStatement->fetchAll(PDO::FETCH_UNIQUE|PDO::FETCH_COLUMN, 1);
    }

    private function execute($statement, $params) {
        $preparedStatement = $this->db->prepare($statement);
        $preparedStatement->execute($params);
    }

    function startTransaction() {
        $this->db->beginTransaction();
    }

    function commitTransaction() {
        $this->db->commit();
    }

    function cancelTransaction() {
        $this->db->rollBack();
    }

    function getArticleGroups() {
        return $this->queryAll('SELECT * FROM articleGroup ORDER BY grade', []);
    }

    function getActiveArticles($groupId) {
        return $this->queryAll('SELECT * FROM article WHERE groupId = ? AND state = "enabled" ORDER BY grade', [ $groupId ]);
    }

    function getActiveArticlesIndexedById() {
        return $this->queryAllIndexedByFirstColumn('SELECT id, name, price FROM article WHERE state = "enabled"', []);
    }

    function getAllArticles($groupId) {
        return $this->queryAll('SELECT * FROM article WHERE groupId = ? ORDER BY grade', [ $groupId ]);
    }

    function getAllArticleNamesIndexedById() {
        return $this->queryAllKeyValue('SELECT id, name FROM article', []);
    }

    function getGroupsOfUsers() {
        $dateCmd = 'DATE_FORMAT(FROM_DAYS(TO_DAYS(NOW()) - TO_DAYS(CONCAT_WS("-",birthdays_year,birthdays_month,birthdays_day))), "%Y") + 0';
        return $this->queryAllGroupedByFirstColumn("SELECT IF($dateCmd < 10, 1, IF($dateCmd < 15, 2, 3)) AS `group`, idm, nachname, vorname, $dateCmd AS age FROM mitglieder ORDER BY `group`, nachname, vorname", []);
    }

    function getUser($userId) {
        return $this->queryOne('SELECT * FROM mitglieder WHERE IDM=:userId', array(":userId" => $userId));
    }

    function getUsers($userIds) {
        if (empty($userIds)) {
            $filter = "1=1";
        } else {
            $filter = "idm IN (" . implode(',', array_map('intval', $userIds)) . ")";
        }
        return $this->queryAll("SELECT idm, CONCAT_WS(' ', nachname, vorname) as full_name, email FROM mitglieder WHERE $filter", []);
    }

    function addOrder($userId, $articleId, $amount, $pricePerUnit, $timestamp, $ipAddress) {
        $statement = 'INSERT INTO orders (id_user, id_order_article, amount, price, `time`, bill_send_time, ip) VALUES(?, ?, ?, ?, ?, ?, ?)';
        $preparedStatement = $this->db->prepare($statement);
        $preparedStatement->execute(array($userId, $articleId, $amount, $pricePerUnit, $timestamp, 0, $ipAddress));
    }

    function getOrders($userId, $timeFrom, $timeTo) {
        $userIdFilter = $userId > 0 ? " AND id_user = :userId" : "";
        $params = [];
        if ($userId > 0) {
            $params[":userId"] = $userId;
        }
        return $this->queryAllGroupedByFirstColumn("SELECT CONCAT(id_order_article, ';', DATE_FORMAT(FROM_UNIXTIME(`time`), '%d.%m.%Y')) as thekey, id_order_article, amount, price, time, bill_send_time FROM orders WHERE `time` >= $timeFrom AND `time` < $timeTo $userIdFilter ORDER BY `time`", $params);
    }

    function getUnsentOrdersBeforeDateForUser($userId, $timeTo) {
        return $this->queryAllGroupedByFirstColumn('SELECT id_order_article, id, amount, price FROM orders WHERE `time` <= ? AND bill_send_time = 0 AND id_user = ?', array($timeTo, $userId));
    }

    function setOrderAsSent($orderId, $sendTime) {
        $preparedStatement = $this->db->prepare('UPDATE orders SET bill_send_time = ? WHERE id = ?');
        $preparedStatement->execute(array($sendTime, $orderId));
    }

    function addArticle($groupId, $state, $grade, $name, $price) {
        $statement = 'INSERT INTO order_articles (id_group, state, grade, name, price) VALUES (?, ?, ?, ?, ?)';
        $preparedStatement = $this->db->prepare($statement);
        $preparedStatement->execute(array($groupId, $state, $grade, $name, $price));
    }

    function updateArticle($articleId, $groupId, $state, $grade, $name, $price) {
        $statement = 'UPDATE order_articles SET id_group = ?, state = ?, grade = ?, name = ?, price = ? WHERE id = ?';
        $preparedStatement = $this->db->prepare($statement);
        $preparedStatement->execute(array($groupId, $state, $grade, $name, $price, $articleId));
    }
}