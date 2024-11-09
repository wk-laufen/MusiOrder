#!/usr/bin/env bash

# 1. Run Raspberry Pi Imager to install Raspberry Pi OS Lite
#   * Set host name, enable SSH, set username/password, configure WIFI, set locale settings
# 2. Run `sudo raspi-config` to setup console autologin and enable wayland

SERVER_URL="http://localhost"
DOCKER_IMAGE_TAG="93faf2f6ccc3dd0b56c81bd6d2cc5a439bd8ae59"

echo "== Install docker ==" # see https://docs.docker.com/engine/install/raspberry-pi-os/#install-using-the-repository
# Add Docker's official GPG key:
sudo apt-get update
sudo apt-get install -y ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/raspbian/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc

# Add the repository to Apt sources:
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/raspbian \
  $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update

sudo apt-get install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

echo "== Setup kiosk mode =="
sudo apt update && sudo apt install --no-install-recommends -y wayfire seatd xdg-user-dirs chromium-browser
echo 'wayland=on' | sudo tee -a '/boot/firmware/cmdline.txt' > /dev/null
# Install hide-cursor plugin
wget https://github.com/seffs/wayfire-plugins-extra-raspbian/releases/download/v0.7.5/wayfire-plugins-extra-raspbian-armv7.tar.xz
mkdir ~/wayfire-plugins-extra-raspbian-armv7
tar xf wayfire-plugins-extra-raspbian-armv7.tar.xz -C wayfire-plugins-extra-raspbian-armv7
sudo cp ~/wayfire-plugins-extra-raspbian-armv7/usr/lib/arm-linux-gnueabihf/libhide-cursor.so /usr/lib/arm-linux-gnueabihf/wayfire/
sudo cp ~/wayfire-plugins-extra-raspbian-armv7/usr/share/wayfire/metadata/hide-cursor.xml /usr/share/wayfire/metadata/

# Create wayfire config
mkdir -p ~/.config
echo "#"'!'"/bin/bash
# Remove exit errors from the config files that could trigger a warning
sed -i 's/\"exited_cleanly\":false/\"exited_cleanly\":true/' ~/.config/chromium/'Local State'
sed -i 's/\"exited_cleanly\":false/\"exited_cleanly\":true/; s/\"exit_type\":\"[^\"]\+\"/\"exit_type\":\"Normal\"/' ~/.config/chromium/Default/Preferences

chromium-browser --noerrdialogs --disable-infobars --kiosk $SERVER_URL" > ~/.config/open-musiorder.sh
chmod +x ~/.config/open-musiorder.sh
echo "[core]
# plugins = autostart hide-cursor # hide-cursor doesn't work
plugins = autostart

[autostart]
musiorder = /home/pi/.config/open-musiorder.sh" > ~/.config/wayfire.ini

echo "[[ -z \$DISPLAY && \$XDG_VTNR -eq 1 ]] && wayfire -c ~/.config/wayfire.ini" >> ~/.bash_profile

echo "== Setup NFC reader =="
sudo apt update && sudo apt install -y pcscd pcsc-tools
echo -e 'install nfc /bin/false\ninstall pn533 /bin/false' | sudo tee /etc/modprobe.d/blacklist.conf
sudo reboot now

echo "== Install Segoe UI =="
git clone https://github.com/mrbvrz/segoe-ui-linux
pushd segoe-ui-linux
chmod +x install.sh
sudo ./install.sh
popd
sudo reboot now

echo "== Create docker configuration =="
mkdir -p ~/musiorder
pushd ~/musiorder
echo "TAG=$DOCKER_IMAGE_TAG" > .env
echo "services:
  nfc-reader:
    image: ghcr.io/wk-laufen/musiorder-nfc-reader:\${TAG}
    restart: unless-stopped
    ports:
      - 8080:8080
    volumes:
      - /run/pcscd/pcscd.comm:/run/pcscd/pcscd.comm" > compose.yml
sudo docker compose up -d
popd
