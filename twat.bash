#reset 
#sync

nmcli radio wifi off
#sleep 1
nmcli networking off
#sleep 1
sudo pkill -9 supplicant
#sudo rmmod ath9k
#sudo modprobe ath9k
#sudo iwconfig wlp2s0 power off 
sudo /sbin/wpa_supplicant -u -s -O /run/wpa_supplicant &
#sleep 1
nmcli networking on
#sleep 1
nmcli radio wifi on
#disown
#sudo tail -f /var/log/syslog

# mighty loop
# while true; do if ping -q -c1 8.8.8.8; then sleep 1 ; else play -q -n synth 0.1 sin 880; sudo /home/john/hobby-code/twat.bash; sleep 15 ; fi ; done

