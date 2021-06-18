CREATE TABLE `Member_New` (
    `id` TEXT NOT NULL PRIMARY KEY,
    `firstName` TEXT NOT NULL,
    `lastName` TEXT NOT NULL,
    `keyCode` TEXT UNIQUE,
    `role` TEXT NOT NULL
);

INSERT INTO `Member_New` SELECT * FROM `Member`;
DROP TABLE `Member`;
ALTER TABLE `Member_New` RENAME TO `Member`;
