#!/usr/bin/env python3

# Von Neumann's Middle Square Method for pseudo-random numbers

f=lambda x:int(('%08d'% (x*x))[2:6])

r=2500;
for i in range(100):
    print(r)
    r=f(r)
    
