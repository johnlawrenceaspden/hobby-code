#!/bin/bash

# this sequence restored comms in the Maypole when it was broken

sudo service network-manager stop
sudo modprobe -r b43 ssb wl mac80211 cfg80211 bcma 
sudo modprobe wl
sudo service network-manager restart
