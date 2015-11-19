#!/usr/bin/env python3

import random

samples=0;
while(samples<100):

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
