/*!40101 SET NAMES utf8 */;

CREATE DATABASE IF NOT EXISTS `order` DEFAULT CHARACTER SET utf8 COLLATE utf8_general_ci;
USE `order`;

DROP TABLE IF EXISTS `member`;
CREATE TABLE IF NOT EXISTS `member` (
  `id` int NOT NULL AUTO_INCREMENT PRIMARY KEY,
  `firstName` varchar(255) NOT NULL,
  `lastName` varchar(255) NOT NULL,
  `keyCode` varchar(255) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;


--

DROP TABLE IF EXISTS `order`;
CREATE TABLE IF NOT EXISTS `order` (
  `id` int NOT NULL AUTO_INCREMENT PRIMARY KEY,
  `userId` int NOT NULL,
  `articleId` int NOT NULL,
  `amount` int NOT NULL,
  `price` decimal NOT NULL,
  `orderedAt` datetime NOT NULL,
  `billedAt` datetime
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--

DROP TABLE IF EXISTS `article`;
CREATE TABLE IF NOT EXISTS `article` (
  `id` int NOT NULL AUTO_INCREMENT PRIMARY KEY,
  `groupId` int NOT NULL,
  `state` ENUM('enabled', 'disabled', 'deleted') NOT NULL,
  `grade` int NOT NULL,
  `name` varchar(255) NOT NULL,
  `price` decimal NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;


--

DROP TABLE IF EXISTS `articleGroup`;
CREATE TABLE IF NOT EXISTS `articleGroup` (
  `id` int NOT NULL AUTO_INCREMENT PRIMARY KEY,
  `grade` int NOT NULL,
  `name` varchar(255) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

