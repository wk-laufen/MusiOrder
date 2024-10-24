#!/usr/bin/env bash

# 1. Run Raspberry Pi Imager to install Raspberry Pi OS Lite
#   * Set host name, enable SSH, set username/password, configure WIFI, set locale settings
# 2. Run `sudo raspi-config` to setup console autologin

SERVER_URL="http://localhost"

echo "== Install docker ==" # see https://docs.docker.com/engine/install/raspberry-pi-os/#install-using-the-repository
# Add Docker's official GPG key:
sudo apt-get update
sudo apt-get install ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/raspbian/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc

# Add the repository to Apt sources:
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/raspbian \
  $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update

sudo apt-get install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

echo "== Setup kiosk mode with Chromium =="
sudo apt update && sudo apt install --no-install-recommends -y xserver-xorg x11-xserver-utils xinit openbox chromium-browser
echo "xset -dpms # Turn off display power management system
xset s noblank # Turn off screen blanking
xset s off # Turn off screen saver

# Remove exit errors from the config files that could trigger a warning
sed -i 's/\"exited_cleanly\":false/\"exited_cleanly\":true/' ~/.config/chromium/'Local State'
sed -i 's/\"exited_cleanly\":false/\"exited_cleanly\":true/; s/\"exit_type\":\"[^\"]\+\"/\"exit_type\":\"Normal\"/' ~/.config/chromium/Default/Preferences

# Run Chromium in kiosk mode
chromium-browser --noerrdialogs --disable-infobars --kiosk \$KIOSK_URL
" | sudo tee -a /etc/xdg/openbox/autostart > /dev/null

echo "export KIOSK_URL=$SERVER_URL" | sudo tee -a /etc/xdg/openbox/environment > /dev/null

echo "[[ -z \$DISPLAY && \$XDG_VTNR -eq 1 ]] && startx -- -nocursor" >> ~/.bash_profile

echo "hdmi_force_hotplug=1
max_usb_current=1
hdmi_drive=1
hdmi_group=2
hdmi_mode=1
hdmi_mode=87
hdmi_cvt 800 480 60 6 0 0 0
dtoverlay=ads7846,cs=1,penirq=25,penirq_pull=2,speed=50000,keep_vref_on=0,swapxy=0,pmax=255,xohms=150,xmin=200,xmax=3900,ymin=200,ymax=3900
display_rotate=0" | sudo tee -a /boot/config.txt > /dev/null

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
