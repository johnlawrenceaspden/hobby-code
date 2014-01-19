#!/bin/bash

# Retrieve all videos for edx's CS-191x course Quantum Computation and Quantum Mechanics
# given their slightly bonkers naming scheme

# wget -c will resume downloads rather than restarting them, so you can run this on a directory 
# containing a partial set to get the rest.

for i in $(seq -w 07 20); do
    for j in $(seq -w 01 05) ; do 
        for k in $(seq -w 00 01); do
            for l in _ __ ; do
                for m in T3 SP ; do
                    filename="BERCS191${m}13-G${i}${j}${k}${l}100.mp4" 
                    echo $filename
                    wget -c https://s3.amazonaws.com/edx-course-videos/berkeley-cs191/${filename}
                done
            done
        done
    done
done
