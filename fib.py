import urllib
import re

websites=["http://homepage.ntlworld.com/john.aspden2/cv.html"]


dictionary={}

for w in websites:
    file=urllib.urlopen(w)
    for x in file.readlines():
        stuff=re.sub(r"<[^>]+>","",x)
        words= stuff.split()
        for wd in words:
            if wd in dictionary:
                dictionary[wd]+=1
            else:
                dictionary[wd]=1

list= [ (v,k) for (k,v) in dictionary.items()]
list.sort()
print list
