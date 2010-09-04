#!/usr/bin/env python

print "hello"

def fib(n):
    if n<2:
        return n
    else:
        return fib(n-1)+fib(n-2)

    
print [fib(x) for x in range(10)]
