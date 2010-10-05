<?php
#CREATE TABLE `track` (
#   `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
#   `did_id` int(10) unsigned NOT NULL,
#   `status` enum('opened','closed') NOT NULL DEFAULT 'opened',
#   `name` varchar(50) NOT NULL,
#   PRIMARY KEY (`id`),
#   KEY `cid_id_idx` (`did_id`),
#   CONSTRAINT `fk_did_id` FOREIGN KEY (`did_id`) REFERENCES `did` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
#) ENGINE=InnoDB AUTO_INCREMENT=281 DEFAULT CHARSET=utf8;

?>
