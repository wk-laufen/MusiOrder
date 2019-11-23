-- phpMyAdmin SQL Dump
-- version 4.5.1
-- http://www.phpmyadmin.net
--
-- Host: localhost:3306
-- Erstellungszeit: 21. Nov 2019 um 21:35
-- Server-Version: 5.1.73-0ubuntu0.10.04.1
-- PHP-Version: 5.5.30

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Datenbank: `tennis`
--
CREATE DATABASE IF NOT EXISTS `tennis` DEFAULT CHARACTER SET utf8 COLLATE utf8_general_ci;
USE `tennis`;

-- --------------------------------------------------------

--
-- Stellvertreter-Struktur des Views `mitglieder`
--
DROP TABLE IF EXISTS `mitglieder`;
CREATE TABLE IF NOT EXISTS `mitglieder` (
`idm` int(10) unsigned
,`email` varchar(254)
,`vorname` varchar(25)
,`nachname` varchar(25)
,`pass` varchar(128)
,`birthdays_year` mediumint(8) unsigned
,`birthdays_month` tinyint(3) unsigned
,`birthdays_day` tinyint(3) unsigned
);

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `orders`
--

DROP TABLE IF EXISTS `orders`;
CREATE TABLE IF NOT EXISTS `orders` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `id_user` int(11) NOT NULL,
  `id_order_article` int(11) NOT NULL,
  `amount` tinyint(4) NOT NULL,
  `price` float NOT NULL,
  `time` tinytext NOT NULL,
  `bill_send_time` tinytext NOT NULL,
  `ip` tinytext NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=23617 DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `order_articles`
--

DROP TABLE IF EXISTS `order_articles`;
CREATE TABLE IF NOT EXISTS `order_articles` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `id_group` int(11) NOT NULL,
  `state` tinyint(1) NOT NULL,
  `grade` int(11) NOT NULL,
  `name` tinytext NOT NULL,
  `price` float NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=73 DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

--
-- Tabellenstruktur für Tabelle `order_article_groups`
--

DROP TABLE IF EXISTS `order_article_groups`;
CREATE TABLE IF NOT EXISTS `order_article_groups` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `grade` int(11) NOT NULL,
  `name` tinytext NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=6 DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
