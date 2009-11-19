#! /usr/bin/env python

import sys, getopt, string

def help_message():
    print '''options.py -- uses getopt to recognize options
Options: -h      displays this help message
     -a      expects an argument
     --file=        expects an argument
     --view  doesn't necessarily expect an argument
     --version displays python version'''
    sys.exit(0)

print 'sys.argv=', sys.argv

try:
    options, xarguments = getopt.gnu_getopt(sys.argv[1:],
            '+ha:', ['file=', 'view', 'version'])
except getopt.error:
    print '(Exception thrown from getopt)'
    print '''Error: You tried to use an unknown option or the 
    argument for an option that requires it was missing. Try 
    \'options.py -h\' for more information.'''
    sys.exit(0)

print 'options=', options
print 'xarguments=', xarguments

for a in options[:]:
    if a[0] == '-h':
        help_message()
for a in options[:]:
    if a[0] == '-a' and a[1] != '':
        print a[0]+' = '+a[1]
        options.remove(a)
        break
    elif a[0] == '-a' and a[1] == '':
        print '-a expects an argument'
        sys.exit(0)
for a in options[:]:
    if a[0] == '--file' and a[1] != '':
        print a[0]+' = '+a[1]
        options.remove(a)
        break
    elif a[0] == '--file' and a[1] == '':
        print '--file expects an argument'
        sys.exit(0)
for a in options[:]:
    if a[0] == '--view' and a[1] != '':
        print a[0]+' = '+a[1]
        options.remove(a)
        break
    elif a[0] == '--view' and a[1] == '':
        print '--view doesn\'t necessarily expects an argument...'
        options.remove(a)
        sys.exit(0)
for a in options[:]:
    if a[0] == '--version':
        print 'options version 0.0.001'
        sys.exit(0)
for a in options[:]:
    if a[0] == '--python-version':
        print 'Python '+sys.version
        sys.exit(0)

        
