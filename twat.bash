#reset 
#sync

nmcli radio wifi off
nmcli networking off
sudo pkill -9 supplicant
sudo rmmod ath9k
sudo modprobe ath9k
#sudo iwconfig wlp2s0 power off 
sudo /sbin/wpa_supplicant -u -s -O /run/wpa_supplicant &
nmcli networking on
nmcli radio wifi on
#disown
#sudo tail -f /var/log/syslog
