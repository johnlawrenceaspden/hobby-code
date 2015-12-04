#!/usr/bin/env python3

import random

twoboxsampleutility=0;
twoboxsamples=0;
oneboxsampleutility=0;
oneboxsamples=0;

samples=0;
while(oneboxsamples<100 and twoboxsamples<100):

    algorithm = random.choice(["twobox","onebox"])

    if algorithm == "twobox":
        output="twobox"
    else:
        output="onebox"

    if output=="twobox":
        omega="empty"
    else:
        omega="fill"

    if output=="twobox":
        action="twobox"
    else:
        action="onebox"

    if action=="twobox":
        if omega=="fill":
            utility=1001000
        else:
            utility=1000
    else:
        if omega=="fill":
            utility=1000000
        else:
            utility=0



    samples=samples+1
    print("algorithm: ", algorithm, "output:", output, "omega:",omega, "action", action, "utility", utility)

    if (action=="twobox"):
        twoboxsampleutility+=utility
        twoboxsamples+=1
    else:
        oneboxsampleutility+=utility
        oneboxsamples+=1

oneboxutility=oneboxsampleutility/oneboxsamples
twoboxutility=twoboxsampleutility/twoboxsamples
        
print ("onebox: ", oneboxutility)
print ("twobox: ", twoboxutility)

if (oneboxutility>=twoboxutility):
    print ("Recommend oneboxing")
else:
    print ("Recommend twoboxing")
        
        
