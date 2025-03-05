# 1. Run Raspberry Pi Imager to install Raspberry Pi OS Lite
#   * Set host name, enable SSH, set username/password, configure WIFI, set locale settings
# 2. Run `sudo raspi-config` to setup console autologin and enable wayland

$HostName = "musiorderpi"
ssh-keygen -R $HostName

ssh pi@$HostName 'mkdir -p ~/musiorder/data'
scp ..\data\data.db pi@${HostName}:/home/pi/musiorder/data/musiorder.db

ssh pi@$HostName 'mkdir -p ~/setup'
scp .\install-docker.sh pi@${HostName}:/home/pi/setup/install-docker.sh
ssh pi@$HostName 'chmod +x ~/setup/install-docker.sh'
scp .\setup.sh pi@${HostName}:/home/pi/setup/setup.sh
ssh pi@$HostName 'chmod +x ~/setup/setup.sh'
ssh pi@$HostName '~/setup/setup.sh'
ssh pi@$HostName 'sudo reboot now'
