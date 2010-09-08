#!/bin/bash

sudo mount /dev/sda6 /mnt/sda6
sudo mount /dev/sda2 /mnt/sda2
sudo mount /dev/sdg /mnt/sdg -o loop,encryption=AES128
sudo mount /dev/sdb1 /mnt/sdb1 -o loop,encryption=AES128
sudo mount /dev/sdb2 /mnt/sdb2 -o loop,encryption=AES128
