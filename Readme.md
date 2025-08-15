# MusiOrder

Website for autonomous ordering of drinks and more.

## Server settings

Settings can be specified as usual, e.g. using `appsettings.json` or environment variables.

* Authentication type
    * Per-user authentication - Order is made per user and user must authenticate
        ```json
        {
          ...
          "AuthHandler": {
            "Name": "AuthenticatedUsers"
          }
        }
        ```
    * No authentication - Order is made per user, but users don't have to authenticate
        ```json
        {
          ...
          "AuthHandler": {
            "Name": "NoAuthentication"
          }
        }
        ```
    * Single user - Order can be made without selecting a specific user
        ```json
        {
          ...
          "AuthHandler": {
            "Name": "SingleUser",
            "User": {
              "Id": "e4470d38-4726-47c4-b7ee-3025992dbf85",
              "Name": "Robin Cella"
            }
          }
        }
        ```

## NFC reader settings

Settings can be specified as usual, e.g. using `appsettings.json` or environment variables.

* Card reader
    * PN532 via UART
        ```json
        {
          ...
          "CardReader": {
            "Type": "pn532-uart",
            "DevicePaths": [
              "/dev/ttyAMA0"
            ]
          }
        }
        ```
    * PC/SC
        ```json
        {
          ...
          "CardReader": {
            "Type": "pcsc"
          }
        }
        ```
    * Console - used in dev environments only to simulate a card reader
        ```json
        {
          ...
          "CardReader": {
            "Type": "console"
          }
        }
        ```

## Client settings

* NFC reader URL
  * If the NFC server happens to run on a different host you can configure the URL in the `localStorage` of the client:
    * Key: `nfc-reader-url`
    * Value: `http://nfc-reader:1234/nfc-reader/card-id`
  * It looks like this must be done manually by opening the site and adding the setting through the Web Developer console.