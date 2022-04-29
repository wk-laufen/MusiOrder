#!/usr/bin/env bash

# Install minimal Raspberry Pi OS with desktop

# Run `sudo raspi-config` and setup Wifi as well as console autologin

apt-get install --no-install-recommends -y xserver-xorg x11-xserver-utils xinit openbox chromium-browser sqlite3

curl -fsSL https://get.docker.com -o get-docker.sh
sh get-docker.sh
usermod -aG docker pi
newgrp docker 
rm get-docker.sh

echo "xset -dpms # Turn off display power management system
xset s noblank # Turn off screen blanking
xset s off # Turn off screen saver

# Remove exit errors from the config files that could trigger a warning
sed -i 's/\"exited_cleanly\":false/\"exited_cleanly\":true/' ~/.config/chromium/'Local State'
sed -i 's/\"exited_cleanly\":false/\"exited_cleanly\":true/; s/\"exit_type\":\"[^\"]\+\"/\"exit_type\":\"Normal\"/' ~/.config/chromium/Default/Preferences

# Run Chromium in kiosk mode
chromium-browser --noerrdialogs --disable-infobars --kiosk \$KIOSK_URL
" >> /etc/xdg/openbox/autostart

echo "export KIOSK_URL=http://localhost" >> /etc/xdg/openbox/environment

echo "[[ -z \$DISPLAY && \$XDG_VTNR -eq 1 ]] && startx -- -nocursor" >> ~/.bash_profile

echo "hdmi_force_hotplug=1
max_usb_current=1
hdmi_drive=1
hdmi_group=2
hdmi_mode=1
hdmi_mode=87
hdmi_cvt 800 480 60 6 0 0 0
dtoverlay=ads7846,cs=1,penirq=25,penirq_pull=2,speed=50000,keep_vref_on=0,swapxy=0,pmax=255,xohms=150,xmin=200,xmax=3900,ymin=200,ymax=3900
display_rotate=0" >> /boot/config.txt
