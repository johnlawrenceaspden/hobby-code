#!/usr/bin/env python

import sys
print sys.argv
for arg in sys.argv[1:]:
    for c in arg:
        print c,"|",
    print
