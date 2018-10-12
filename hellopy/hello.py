#!/usr/bin/env python

# Load into Emacs, should cause elpy mode, use C-c C-c to run in a repl window

print "hello"

def fib(n):
    if n<2:
        return n
    else:
        return fib(n-1)+fib(n-2)

    
print [fib(x) for x in range(10)]
