#!/usr/bin/env python

def fib(n):
    if n==0: return 0
    if n==1: return 1
    else:
        print "fib",n,
        return fib(n-1)+fib(n-2)

def calc(f,n):
    print f,'(',n,')'
    print f(n)


print "We have defined fib using the mathematical definition"
print 'but it is inefficent:'
calc(fib,10)

print "and it doesn't get better with practise"
calc(fib,10)

from closurememoize import memoize

newfib=memoize(fib)
print "here's a version that remembers what it's done"
calc(newfib,10)
print "its second go is faster than its first"
calc(newfib,10)

print "here we replace the original function with its memoized self:"

fib=memoize(fib)

print "suddenly it's as quick as the iterative algorithm"
calc(fib,10)
print "and it remembers past computations"
calc(fib,13)
