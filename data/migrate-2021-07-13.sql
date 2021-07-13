ALTER TABLE `Member` ADD COLUMN `deleteTimestamp` TEXT;
CREATE VIEW `ActiveMember` AS SELECT `id`, `firstName`, `lastName`, `keyCode`, `role` FROM `Member` WHERE `deleteTimestamp` IS NULL
