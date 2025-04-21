CREATE TABLE AuthKey (
    keyCode TEXT NOT NULL,
    keyType TEXT NOT NULL,
    userId TEXT NOT NULL REFERENCES Member(id),
    creationTime TEXT NOT NULL,
    PRIMARY KEY (keyCode, keyType)
);

INSERT INTO AuthKey (keyCode, keyType, userId)
SELECT keyCode, 'nfc', id FROM Member WHERE keyCode NOT NULL;

-- see https://sqlite.org/lang_altertable.html - Making Other Kinds Of Table Schema Changes
PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
DROP VIEW ActiveMember;
CREATE TABLE Member2 (
    id TEXT NOT NULL PRIMARY KEY,
    firstName TEXT NOT NULL,
    lastName TEXT NOT NULL,
    role TEXT NOT NULL,
    deleteTimestamp TEXT
);
INSERT INTO Member2 (id, firstName, lastName, role, deleteTimestamp) SELECT id, firstName, lastName, role, deleteTimestamp FROM Member;
DROP TABLE Member;
ALTER TABLE Member2 RENAME TO Member;
CREATE VIEW ActiveMember AS SELECT id, firstName, lastName, (SELECT json_group_array(json_object('keyCode', keyCode, 'keyType', keyType)) FROM AuthKey WHERE userId = id) AS authKeys, role FROM Member WHERE deleteTimestamp IS NULL;
PRAGMA foreign_key_check;
COMMIT TRANSACTION;
PRAGMA foreign_keys=ON;
