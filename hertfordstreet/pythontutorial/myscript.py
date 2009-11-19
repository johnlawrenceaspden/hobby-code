#! /usr/bin/env python

import os

# Fibonacci series

def fib(n):
	"""Print a fibonnaci series up to n."""
	a,b = 0,1
	while b<n:
		print b,
		a,b = b, a+b

fib(3000)
