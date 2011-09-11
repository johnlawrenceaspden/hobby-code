#!/usr/bin/env python

# convert binary file to hex , one byte per line
import sys
f=sys.stdin
#with open("zxsin.png", "rb") as f:
byte = f.read(1)
while byte:
    print "%02x " % ord(byte)
    byte = f.read(1)
