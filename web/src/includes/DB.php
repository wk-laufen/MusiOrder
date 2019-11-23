<?php

class DB {
    private $db;

    function DB($host, $username, $password, $dbName) {
        $this->db = new mysqli($host, $username, $password, $dbName);
        if ($this->db->connect_errno) {
            die("Failed to connect to database: (" . $this->db->connect_errno . ") " . $this->db->connect_error);
        }
    }

    function queryAll($statement) {
        $result = $this->db->query($statement);
        if (!$result) {
            return false;
        }
        return $result->fetch_all(MYSQLI_ASSOC);
    }

    function queryAllIndexByCol($statement, $colName) {
        $rows = [];
        $result = $this->db->query($statement);
        if (!$result) {
            return false;
        }
        while ($row = $result->fetch_assoc()) {
            $rows[$row[$colName]] = $row;
        }
        return $rows;
    }
}