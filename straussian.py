#!/usr/bin/env python3

import random;

bankroll=100

for _ in range(100):

    headsbiased=random.choice([True, False])

    c=["H","H","T"] if  headsbiased else ["H","T","T"];
    l=random.choices(c,k=1);

    print("results",l)

    bits=l.count("H")-l.count("T")

    print("bits",bits)

    if bits>=0:
        print("heads-biased at", 2**bits," to 1")
    else:
        print("tails-biased at",2**(-bits),"to 1")

    p=2**bits/(1+2**bits)
    print("p=",p)

    kelly=(p*2-1)/(2-1)

    print("kelly fraction", kelly)

    bet=bankroll*kelly

    print( "bankroll:" ,bankroll, "bet:", bet)

    if bits>=0 and headsbiased:
        print("yay")
        bankroll=bankroll+bet
    else:
        print("oops")
        bankroll=bankroll-bet


    print("true coin", c)
    print("new bankroll",bankroll)

