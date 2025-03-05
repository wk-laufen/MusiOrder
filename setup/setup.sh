#!/usr/bin/env bash

SERVER_URL="http://localhost"
DOCKER_IMAGE_TAG="e816f44850b31bb01ef75ec3f252bb1622a9632a"
SCREEN_RESOLUTION="1280x800"

echo "== Install docker =="
pushd $(dirname "$0") > /dev/null
./install-docker.sh
popd > /dev/null

echo "== Install wayfire =="
sudo apt update && sudo apt install --no-install-recommends -y wayfire seatd xdg-user-dirs
echo -n " wayland=on video=HDMI-A-1:${SCREEN_RESOLUTION}M@60D" | sudo tee -a /boot/firmware/cmdline.txt > /dev/null

echo "== Install firefox and utilities =="
sudo apt update && sudo apt install -y firefox firefox-l10n-de git sqlite3 jq

echo "== Create firefox config =="
mkdir -p ~/.mozilla/firefox/Profiles/kiosk
echo "[Profile0]
Name=kiosk
IsRelative=1
Path=Profiles/kiosk" > ~/.mozilla/firefox/profiles.ini
echo "user_pref('browser.download.useDownloadDir', false);
user_pref('browser.download.dir', '/usb');
user_pref('intl.locale.requested', 'de');" > ~/.mozilla/firefox/Profiles/kiosk/user.js

echo "== Create wayfire config =="
mkdir -p ~/.config
echo "#"'!'"/bin/bash
while ! curl $SERVER_URL; do sleep 1; done
firefox -kiosk -P kiosk $SERVER_URL" > ~/.config/open-musiorder.sh
chmod +x ~/.config/open-musiorder.sh
echo "[core]
plugins = autostart

[output:HDMI-A-1]
mode = $SCREEN_RESOLUTION@60

[autostart]
musiorder = /home/pi/.config/open-musiorder.sh" > ~/.config/wayfire.ini

echo "[[ -z \$DISPLAY && \$XDG_VTNR -eq 1 ]] && wayfire -c ~/.config/wayfire.ini" >> ~/.bash_profile

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

echo "== Create docker configuration =="
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
      - AuthHandler__Name=NoAuthentication
    volumes:
      - ./data/musiorder.db:/app/data/musiorder.db
  nfc-reader:
    image: ghcr.io/wk-laufen/musiorder-nfc-reader:\${TAG}
    restart: unless-stopped
    ports:
      - 8080:8080
    volumes:
      - /run/pcscd/pcscd.comm:/run/pcscd/pcscd.comm" > compose.yml
sudo docker compose up -d
popd
