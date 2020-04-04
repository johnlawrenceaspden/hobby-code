reset 
sync
sudo pkill -9 supplicant
sudo rmmod wl 
sudo modprobe wl 
sudo iwconfig wlp2s0 power off 
sudo /sbin/wpa_supplicant -u -s -O /run/wpa_supplicant &
disown
sudo tail -f /var/log/syslog
