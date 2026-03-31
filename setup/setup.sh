#!/usr/bin/env bash

SERVER_URL="http://localhost"
DOCKER_IMAGE_TAG="a30da4a4d12d8df47e676fe2b92065d4eb676880"
SCREEN_RESOLUTION="1280x800"

echo "== Install utils =="
sudo apt update && sudo apt install -y git sqlite3 jq

echo "== Install docker ==" # see https://docs.docker.com/engine/install/raspberry-pi-os/#install-using-the-repository
# Add Docker's official GPG key:
sudo apt-get update
sudo apt-get install -y ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/debian/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc
# Add the repository to Apt sources:
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/debian \
  $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update
# Install packages
sudo apt-get install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

echo "== Install firefox =="
sudo apt update && sudo apt install -y firefox firefox-l10n-de

echo "== Create firefox config =="
mkdir -p ~/.mozilla/firefox/Profiles/kiosk
echo "[Profile0]
Name=kiosk
IsRelative=1
Path=Profiles/kiosk" > ~/.mozilla/firefox/profiles.ini
echo "user_pref('browser.download.useDownloadDir', false);
user_pref('browser.download.dir', '/usb');
user_pref('intl.locale.requested', 'de');" > ~/.mozilla/firefox/Profiles/kiosk/user.js

echo "== Setup labwc =="
sudo apt update && sudo apt install -y labwc
mkdir -p ~/.config/labwc
echo "#while ! curl $SERVER_URL; do sleep 1; done
firefox --kiosk -P kiosk $SERVER_URL &" > ~/.config/labwc/autostart
echo "[[ -z \$DISPLAY && \$XDG_VTNR -eq 1 ]] && labwc" >> ~/.bash_profile

echo "== Auto-mount USB drives =="
sudo mkdir -p /usb && sudo chown pi:pi /usb
pushd $(dirname "$0") > /dev/null
git clone https://github.com/antonilol/simple-usb-automount
pushd ./simple-usb-automount
sudo ./install
sed 's/mntopts="/&uid=pi,gid=pi,/' /usr/local/bin/simple-usb-automount | sudo tee /usr/local/bin/simple-usb-automount > /dev/null
popd > /dev/null
popd > /dev/null

echo "== Setup NFC reader =="
sudo apt update && sudo apt install -y pcscd pcsc-tools
echo -e 'install nfc /bin/false\ninstall pn533 /bin/false' | sudo tee /etc/modprobe.d/blacklist.conf

echo "== Install Segoe UI =="
pushd $(dirname "$0") > /dev/null
git clone https://github.com/mrbvrz/segoe-ui-linux
pushd ./segoe-ui-linux
FONT_DIR="$HOME/.local/share/fonts/Microsoft/TrueType/SegoeUI/"
mkdir -p $FONT_DIR
find font/ -type f -name '*.ttf' -exec cp '{}' $FONT_DIR ';'
fc-cache -f $FONT_DIR
popd > /dev/null
popd > /dev/null

echo "== Option 1: Create docker configuration =="
pushd ~/musiorder
echo "TAG=$DOCKER_IMAGE_TAG" > .env
echo "services:
  app:
    image: ghcr.io/wk-laufen/musiorder:\${TAG}
    restart: unless-stopped
    ports:
      - 80:8080
    environment:
      - DB_PATH=/app/data/musiorder.db
      - AuthHandler__Name=AuthenticatedUsers
    volumes:
      - ./data/musiorder.db:/app/data/musiorder.db
  nfc-reader:
    image: ghcr.io/wk-laufen/musiorder-nfc-reader:\${TAG}
    restart: unless-stopped
    ports:
      - 8080:8080
    environment:
      - CardReader__Type=pcsc
    volumes:
      - /run/pcscd/pcscd.comm:/run/pcscd/pcscd.comm" > compose.yml
sudo docker compose up -d
popd

# echo "== Option 2: Add systemd services"
# echo '[Unit]
# Description=MusiOrderApp
# After=network.target
# StartLimitIntervalSec=0

# [Service]
# Type=simple
# Restart=always
# RestartSec=1
# User=pi
# Environment="DB_PATH=/home/pi/musiorder/data/musiorder.db"
# Environment="AuthHandler__Name=AuthenticatedUsers"
# WorkingDirectory=/home/pi/musiorder/app
# ExecStart=/home/pi/musiorder/app/MusiOrder.Server

# [Install]
# WantedBy=multi-user.target' | sudo tee /etc/systemd/system/musiorder-app.service > /dev/null
# sudo systemctl enable musiorder-app

# echo '[Unit]
# Description=MusiOrderNfcReader
# After=network.target
# StartLimitIntervalSec=0

# [Service]
# Type=simple
# Restart=always
# RestartSec=1
# User=pi
# ExecStart=/home/pi/musiorder/nfc-reader/MusiOrder.NfcReader

# [Install]
# WantedBy=multi-user.target' | sudo tee /etc/systemd/system/musiorder-nfcreader.service > /dev/null
# sudo systemctl enable musiorder-nfcreader
