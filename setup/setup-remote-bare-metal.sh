#!/bin/bash

# 1. Run Raspberry Pi Imager to install Raspberry Pi OS Lite
#   * Set host name, enable SSH, set username/password, configure WIFI, set locale settings
# 2. Run `sudo raspi-config` to setup console autologin and enable wayland

HostName="musiorderpi"
ssh-keygen -R $HostName

ssh pi@$HostName "mkdir -p ~/musiorder/data"
scp ../data/musiorder.db pi@${HostName}:/home/pi/musiorder/data/musiorder.db

bash ./build.sh
scp ../out.tar.gz pi@${HostName}:/home/pi/musiorder/out.tar.gz
ssh pi@$HostName "tar -xvzf ~/musiorder/out.tar.gz -C ~/musiorder"
scp ./setup.sh pi@${HostName}:/home/pi/setup/setup.sh
ssh pi@$HostName "chmod +x ~/setup/setup.sh"
ssh pi@$HostName "~/setup/setup.sh"
ssh pi@$HostName "sudo reboot now"
