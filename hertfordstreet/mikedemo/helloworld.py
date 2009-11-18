#! /usr/bin/env python2.4

import re,urllib
from re import sub

file=urllib.urlopen("http://homepage.ntlworld.com/john.aspden2/cv.html")
dict={}
rawdict={}
f=file.readlines()
for l in f:
    raw=l.split()
    for w in raw:
        rawdict[w]=rawdict.get(w,0)+1
    l=re.sub(r'<.*?>',' ',l) #remove html tags
    l=re.sub(r'&.*?;',' ',l) #remove html specials like &nbsp;
    l=re.sub(r"[,\(,\),\.!]",' ',l) #remove punctuation
    l=l.split()
    for w in l:
        dict[w]=dict.get(w,0)+1
    
file.close()
#print dict
l=dict.items()
rl=[(b,a) for (a,b) in l]
rl.sort()
#for (a,b) in rl:
#   print b.rjust(20), ":", a
#print rl

print "differences between counts with and without filtering"
print "words which occur in both sets with different numbers"
wset=set(dict)
rawset=set(rawdict)
intersection=wset.intersection(rawset);
for i in intersection:
    if dict[i]!=rawdict[i]:
        print i, dict[i], rawdict[i], "                  ",
        for r in rawset:
            if r.find(i)!=-1:
                print r, rawdict[r],
        print

print "words which appear in only the processed set"

for i in wset-intersection:
    print i, dict[i]

print "words which appear in only the raw set"
for i in rawset-intersection:
   print i, rawdict[i]



