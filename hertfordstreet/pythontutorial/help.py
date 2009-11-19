#! /usr/bin/env python

import sys

if '-h' in sys.argv or '--help' in sys.argv or '--help' in sys.argv:
	print '''
help.py--does nothing useful (yet)
options: -h -help or --help  
	display this help
		'''
	sys.exit(0)
else:
	print 'I don\'t recognize this option' 
	sys.exit(0)
