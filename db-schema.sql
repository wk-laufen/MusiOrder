DROP TABLE IF EXISTS `Order`;
DROP TABLE IF EXISTS `Article`;
DROP TABLE IF EXISTS `ArticleGroup`;
DROP TABLE IF EXISTS `MemberPayment`;
DROP VIEW IF EXISTS `ActiveMember`;
DROP TABLE IF EXISTS `AuthKey`;
DROP TABLE IF EXISTS `Member`;

CREATE TABLE `Member` (
    `id` TEXT NOT NULL PRIMARY KEY,
    `firstName` TEXT NOT NULL,
    `lastName` TEXT NOT NULL,
    `role` TEXT NOT NULL,
    `deleteTimestamp` TEXT
);

CREATE TABLE AuthKey (
    keyCode TEXT NOT NULL,
    keyType TEXT NOT NULL,
    userId TEXT NOT NULL REFERENCES Member(id),
    creationTime TEXT NOT NULL,
    PRIMARY KEY (keyCode, keyType)
);

CREATE VIEW ActiveMember AS SELECT id, firstName, lastName, (SELECT json_group_array(json_object('keyCode', keyCode, 'keyType', keyType)) FROM AuthKey WHERE userId = id) AS authKeys, role FROM Member WHERE deleteTimestamp IS NULL;

CREATE TABLE `MemberPayment` (
    `id` TEXT NOT NULL PRIMARY KEY,
    `userId` TEXT NOT NULL REFERENCES member(id),
    `amount` TEXT NOT NULL,
    `timestamp` TEXT NOT NULL
);

CREATE TABLE `ArticleGroup` (
    `id` TEXT NOT NULL PRIMARY KEY,
    `grade` INTEGER NOT NULL,
    `name` TEXT NOT NULL
);

CREATE TABLE `Article` (
    `id` TEXT NOT NULL PRIMARY KEY,
    `groupId` TEXT NOT NULL REFERENCES articleGroup(id),
    `state` TEXT NOT NULL,
    `grade` INTEGER NOT NULL,
    `name` TEXT NOT NULL,
    `price` TEXT NOT NULL
);

CREATE TABLE `Order` (
    `id` TEXT NOT NULL PRIMARY KEY,
    `userId` TEXT NOT NULL REFERENCES member(id),
    `articleName` TEXT NOT NULL,
    `amount` INTEGER NOT NULL,
    `pricePerUnit` TEXT NOT NULL,
    `timestamp` TEXT NOT NULL
);
