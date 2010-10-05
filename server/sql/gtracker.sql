-- MySQL dump 10.13  Distrib 5.1.41, for debian-linux-gnu (x86_64)
--
-- Host: localhost    Database: gtracker
-- ------------------------------------------------------
-- Server version	5.1.41-3ubuntu12.3

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `coordinate`
--

DROP TABLE IF EXISTS `coordinate`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `coordinate` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `track_id` int(10) unsigned NOT NULL,
  `device_id` int(10) unsigned NOT NULL,
  `latitude` double DEFAULT NULL,
  `longitude` double DEFAULT NULL,
  `speed` int(10) unsigned NOT NULL DEFAULT '0',
  `time` timestamp NOT NULL DEFAULT '0000-00-00 00:00:00',
  `timestamp` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  KEY `track_id_idx` (`track_id`) USING BTREE,
  KEY `device_id_idx` (`device_id`) USING BTREE,
  CONSTRAINT `FK_coordinate_track_id` FOREIGN KEY (`track_id`) REFERENCES `track` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=312527 DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `device`
--

DROP TABLE IF EXISTS `device`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `device` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `name` char(12) NOT NULL,
  `alias` varchar(50) DEFAULT NULL,
  `online` tinyint(1) NOT NULL,
  `timezone` varchar(128) NOT NULL DEFAULT 'Europe/Moscow',
  `registered` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `color` varchar(6) DEFAULT '000000',
  `weight` int(11) DEFAULT '3',
  `pixmap` varchar(16) DEFAULT NULL,
  `twitter_auth` text,
  PRIMARY KEY (`id`),
  UNIQUE KEY `value` (`name`)
) ENGINE=InnoDB AUTO_INCREMENT=427 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `lang`
--

DROP TABLE IF EXISTS `lang`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `lang` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `lang` char(2) NOT NULL,
  `name` varchar(128) DEFAULT NULL,
  `value` varchar(256) DEFAULT NULL,
  `extern` tinyint(1) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=242 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `meta`
--

DROP TABLE IF EXISTS `meta`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `meta` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `lang_id` int(11) NOT NULL,
  `name` varchar(128) DEFAULT NULL,
  `value` text,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `news`
--

DROP TABLE IF EXISTS `news`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `news` (
  `date` datetime DEFAULT NULL,
  `post` text
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `reference`
--

DROP TABLE IF EXISTS `reference`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `reference` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `device_id` int(10) unsigned NOT NULL,
  `track_id` int(10) unsigned DEFAULT NULL,
  `value` varchar(32) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `FK_reference_device_id` (`device_id`),
  KEY `FK_reference_track_id` (`track_id`),
  CONSTRAINT `FK_reference_device_id` FOREIGN KEY (`device_id`) REFERENCES `device` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `FK_reference_track_id` FOREIGN KEY (`track_id`) REFERENCES `track` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=8 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `track`
--

DROP TABLE IF EXISTS `track`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `track` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `device_id` int(10) unsigned NOT NULL,
  `status` enum('opened','closed') NOT NULL DEFAULT 'opened',
  `name` varchar(50) NOT NULL,
  `avg_speed` int(10) unsigned DEFAULT NULL,
  `length` int(10) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `FK_track_device_id` (`device_id`),
  CONSTRAINT `FK_track_device_id` FOREIGN KEY (`device_id`) REFERENCES `device` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=453 DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `trigger`
--

DROP TABLE IF EXISTS `trigger`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `trigger` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `device_id` int(10) unsigned NOT NULL,
  `enabled` tinyint(1) NOT NULL DEFAULT '0',
  `name` varchar(128) CHARACTER SET cp1251 NOT NULL,
  `type` enum('online','offline','enter','leave','sos','periodic') DEFAULT NULL,
  `use_email` tinyint(1) NOT NULL DEFAULT '0',
  `email` varchar(256) DEFAULT NULL,
  `use_phone` tinyint(1) NOT NULL DEFAULT '0',
  `phone` varchar(256) DEFAULT NULL,
  `use_twitter` tinyint(1) DEFAULT '0',
  `text` text CHARACTER SET cp1251,
  `config` varchar(256) DEFAULT NULL,
  `schedule` text,
  PRIMARY KEY (`id`),
  KEY `FK_trigger_device_id` (`device_id`),
  CONSTRAINT `FK_trigger_device_id` FOREIGN KEY (`device_id`) REFERENCES `device` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=7 DEFAULT CHARSET=latin1 ROW_FORMAT=DYNAMIC;
/*!40101 SET character_set_client = @saved_cs_client */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2010-07-17 11:00:02
