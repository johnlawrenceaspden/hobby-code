#!/usr/bin/env python

# Run with
# ./debug-test.py
# python ./debug-test.py

# Command line debug 
# python -m pdb debug-test.py
# type help for commands

# Emacs debugger M-x pdb
# Run pdb (like this): python -m pdb /home/jla/solar_capture/debug-test.py
# goto *gud-pdb* window 

# Or maybe better to use realgud
# https://github.com/realgud/realgud/
# M-x package-install RET realgud RET
# M-x load-library RET realgud RET

x=2

#hello

for i in range(10):
    print x
    print "hello"
    print "world"

print "bye"


