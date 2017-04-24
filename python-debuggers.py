#!/usr/bin/env python

# Run with
# ./python-debuggers.py
# python ./python-debuggers.py

# Command line debug 
# python -m pdb python-debuggers.py
# type help for commands

# PUDB
# There's a nice curses-based debugger called pudb
# python -m pudb ./python-debuggers.py

# Emacs debugger M-x pdb
# Run pdb (like this): python -m pdb /home/jla/hobby-code/python-debuggers.py
# goto *gud-pdb* window 

# Or maybe better to use realgud
# https://github.com/realgud/realgud/
# M-x package-install RET realgud RET
# M-x load-library RET realgud RET
# and then as above.

x=2


class aclass(object):
    def __init__(self, val):
        self.data=val
    def __repr__(self):
        return "aclass({0.data})".format(self)


class bclass(aclass):
    def __init__(self, val, val2):
        self.moredata=val2
        super(bclass,self).__init__(val)
    def __repr__(self):
        return "bclass({0.data}, {0.moredata})".format(self)

    


def factorial(n):
    if n<=1 :
        return 1
    else:
        return n * factorial(n-1)

#hello

for i in range(10):
    print i
    print "hello"
    print factorial(i)
    print "world"


a=aclass(2)
print a
b=bclass(3,7)
print b

    
print "bye"


