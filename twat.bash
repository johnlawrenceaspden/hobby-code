#reset 
#sync

nmcli radio wifi off
#sleep 1
nmcli networking off
#sleep 1
sudo pkill -9 supplicant
sudo rmmod ath9k
sudo modprobe ath9k
#sudo iwconfig wlp2s0 power off 
sudo /sbin/wpa_supplicant -u -s -O /run/wpa_supplicant &
#sleep 1
nmcli networking on
#sleep 1
nmcli radio wifi on
#disown
#sudo tail -f /var/log/syslog
