#!/usr/bin/env python2.5

import glob
import os
import re

email_pattern=re.compile(r'\b[A-Za-z][A-Za-z0-9._%-]*@aspden\.com')

print "Hello"

def getemails(file):
        return email_pattern.findall(open(file).read())

##        for l in open(file):
##            print l


dict={}

files=[f for f in glob.glob("*") if os.path.isfile(f)]
nfiles=len(files)
count=0

for file in files:
    count+=1
    print count, "/", nfiles, file
    emails=getemails(file)
    for i in emails:
        dict[i]=dict.get(i,0)+1

    print emails

for i in dict.keys():
    print i,dict[i]
    
