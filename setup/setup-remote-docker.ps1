# 1. Run Raspberry Pi Imager to install Raspberry Pi OS Lite
#   * Use Bookworm as with newer versions there might be mismatches with PCSC libs in NFC reader docker image
#   * Set host name, enable SSH, set username/password, configure WIFI, set locale settings
# 2. Run `sudo raspi-config` to setup console autologin and enable wayland

$HostName = "192.168.0.18"
ssh-keygen -R $HostName

ssh pi@$HostName "mkdir -p ~/musiorder/data"
scp ~/sync/johannes/Development/MusiOrder_Backups/Siebenbuerger_Vorchdorf/musiorder.2025.11.22.db pi@${HostName}:/home/pi/musiorder/data/musiorder.db

ssh pi@$HostName "mkdir -p ~/setup"
scp ./setup.sh pi@${HostName}:/home/pi/setup/setup.sh
ssh pi@$HostName "chmod +x ~/setup/setup.sh"
ssh pi@$HostName "~/setup/setup.sh"
ssh pi@$HostName "sudo reboot now"
