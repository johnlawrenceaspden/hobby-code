#!/usr/bin/env python

def factorial(n):
    if n==1 :
        return 1
    else:
        import pdb
        pdb.set_trace()
        return n * factorial(n-1)

