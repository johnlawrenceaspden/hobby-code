#!/bin/bash

# this sequence restored comms in the Maypole when it was broken
# when typed in, but when run as a script seem to break it

# added short pauses to see if that helps

sudo service network-manager stop
sleep 4
sudo modprobe -r b43 ssb wl mac80211 cfg80211 bcma 
sleep 4
sudo modprobe wl
sleep 4
sudo service network-manager restart
