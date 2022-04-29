INSERT INTO Member (`id`, `firstName`, `lastName`, `keyCode`, `role`) VALUES ('e4470d38-4726-47c4-b7ee-3025992dbf85', 'Robin', 'Cella', '1234', 'admin')
INSERT INTO Member (`id`, `firstName`, `lastName`, `keyCode`, `role`) VALUES ('ef87d330-94ab-4ea5-bfb0-bce4329fd3a7', 'Sebastian', 'Manz', 'qwer', 'user')
INSERT INTO Member (`id`, `firstName`, `lastName`, `keyCode`, `role`) VALUES ('b018c560-af6d-42d0-9ee7-4aa46a764a47', 'Thomas', 'Morgenstern', 'asdf', 'user')
INSERT INTO Member (`id`, `firstName`, `lastName`, `keyCode`, `role`) VALUES ('c421dfe2-af42-4770-bef3-b8a7c06917c5', 'Matheo', 'Goss', 'zxcv', 'user')
INSERT INTO Member (`id`, `firstName`, `lastName`, `keyCode`, `role`) VALUES ('e5db874a-dc7f-407a-9a37-eb1e18dcf1be', 'Dominik', 'Haspel', 'wert', 'user')

INSERT INTO ArticleGroup (`id`, `grade`, `name`) VALUES ('1e11c985-cb94-4ca4-ace9-4752f43c314a', 1, 'Getränke')
INSERT INTO ArticleGroup (`id`, `grade`, `name`) VALUES ('1f418489-6b00-46d9-9d76-25a691a10e11', 2, 'Speisen')

INSERT INTO Article (`id`, `groupId`, `state`, `grade`, `name`, `price`) VALUES ('9f33f4ed-785d-45d0-a385-5990c99d2b74', '1e11c985-cb94-4ca4-ace9-4752f43c314a', 'enabled', 1, 'Bier', 2.5)
INSERT INTO Article (`id`, `groupId`, `state`, `grade`, `name`, `price`) VALUES ('83646d9d-ac2d-4fd0-9633-abf884527fa6', '1e11c985-cb94-4ca4-ace9-4752f43c314a', 'enabled', 2, 'Wasser', 20)
INSERT INTO Article (`id`, `groupId`, `state`, `grade`, `name`, `price`) VALUES ('291da6e7-411a-47f2-b1d1-38caeae10a86', '1e11c985-cb94-4ca4-ace9-4752f43c314a', 'enabled', 3, 'Saft', 10)
INSERT INTO Article (`id`, `groupId`, `state`, `grade`, `name`, `price`) VALUES ('2cc84f35-65e7-4ee1-a57b-dca95cf0d9c6', '1e11c985-cb94-4ca4-ace9-4752f43c314a', 'enabled', 4, 'Wein', 3)
INSERT INTO Article (`id`, `groupId`, `state`, `grade`, `name`, `price`) VALUES ('dc8beb05-b98c-4006-b0e3-d05d529b2562', '1e11c985-cb94-4ca4-ace9-4752f43c314a', 'disabled', 5, 'alter Wein', 2.3)

INSERT INTO Article (`id`, `groupId`, `state`, `grade`, `name`, `price`) VALUES ('67c47193-08c7-4e9b-904c-2807668ae73c', '1f418489-6b00-46d9-9d76-25a691a10e11', 'enabled', 1, 'Käseburger', 5.2)
INSERT INTO Article (`id`, `groupId`, `state`, `grade`, `name`, `price`) VALUES ('8e83fd8d-1a93-41cc-981b-2f532cf9e904', '1f418489-6b00-46d9-9d76-25a691a10e11', 'enabled', 2, 'Schinkenburger', 4.7)

INSERT INTO MemberPayment (`id`, `userId`, `amount`, `timestamp`) VALUES ('aa85847b-a3d2-4015-b486-a65536e9c49f', 'e4470d38-4726-47c4-b7ee-3025992dbf85', 10, '2020-07-04 13:55:20.0263132+02:00')
INSERT INTO MemberPayment (`id`, `userId`, `amount`, `timestamp`) VALUES ('bab60ee2-4241-4dad-ab15-36ad4b387079', 'e4470d38-4726-47c4-b7ee-3025992dbf85', 2.5, '2020-06-04 19:55:20.0263132+02:00')
INSERT INTO MemberPayment (`id`, `userId`, `amount`, `timestamp`) VALUES ('aefcb588-c972-4bfb-b66e-df18ec4af02f', 'ef87d330-94ab-4ea5-bfb0-bce4329fd3a7', 10, '2020-07-04 13:55:20.0263132+02:00')
