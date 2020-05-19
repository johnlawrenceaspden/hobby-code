
nmcli radio wifi off

sleep 1
sudo pkill -9 supplicant
sudo rmmod ath9k
sudo modprobe ath9k

sudo /sbin/wpa_supplicant -B -u -s -c/etc/wpa_supplicant.conf -O /run/wpa_supplicant &

nmcli radio wifi on





#sudo iwconfig wlp2s0 power off 

#nmcli connection down eduroam
#nmcli connection up eduroam

#nmcli networking off
#nmcli networking on

#sudo /sbin/wpa_supplicant -dd -u -s -O /run/wpa_supplicant &
#disown
#sudo tail -f /var/log/syslog


# mighty loop
# while true; do if ping -q -c1 8.8.8.8; then sleep 1 ; else play -q -n synth 0.1 sin 880; sudo /home/john/hobby-code/twat.bash; sleep 15 ; fi ; done

