#!/bin/bash
set -x

sudo ip link
sudo iwconfig
sudo nm-tool

# this sequence restored comms in the Maypole when it was broken
# when typed in, but when run as a script broke it irrevocably

# it (typed in) also cures the problem in the boathouse if the wireless wakes up broken

# reboot and it still didn't work, but then try this sequence again
# and all was suddenly well.

# added short pauses to see if that helps

# Sometimes you get Association Request to the driver failed
# sometimes it's deauth request to the driver failed

# This repeatedly in syslog
#Apr  4 15:16:25 dell-mini wpa_supplicant[1002]: Deauth request to the driver failed
#Apr  4 15:16:25 dell-mini NetworkManager[2939]: <info> (eth2): supplicant interface state: associating -> disconnected
#Apr  4 15:16:25 dell-mini NetworkManager[2939]: <info> (eth2): supplicant interface state: disconnected -> scanning

# repeated for ages then all sorts of guff about changing regulatory domain to TW
# then makes connection quickly

# It seems if it's in this state then these commands will cure it.


sudo service network-manager stop
sleep 4
sudo service networking stop
sleep 4
sudo modprobe -r b43 ssb wl mac80211 cfg80211 bcma 
sleep 4
sudo modprobe wl
sleep 4
sudo service networking start
sleep 4
sudo service network-manager start
sleep 10

sudo ip link
sudo iwconfig
sudo nm-tool
echo 'other things to try'
echo 'sudo pkill -f wpa_supplicant'
echo 'use dmesg | grep "eth\|wl\|net\|802"'

