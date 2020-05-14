set +x
#reset 

#sync

#nmcli connection down eduroam
nmcli radio wifi off
#nmcli networking off
sleep 1
sudo pkill -9 supplicant
sudo rmmod ath9k
sudo modprobe ath9k
#sudo iwconfig wlp2s0 power off 
sudo /sbin/wpa_supplicant -B -u -s -c/etc/wpa_supplicant.conf -O /run/wpa_supplicant &
#sudo /sbin/wpa_supplicant -dd -u -s -O /run/wpa_supplicant &
#disown
#nmcli networking on
nmcli radio wifi on
#nmcli connection up eduroam
#sudo tail -f /var/log/syslog

# mighty loop
# while true; do if ping -q -c1 8.8.8.8; then sleep 1 ; else play -q -n synth 0.1 sin 880; sudo /home/john/hobby-code/twat.bash; sleep 15 ; fi ; done

