#!/usr/bin/env python

'''reenable stopped searches in gtk-gnutella by hacking the configuration files
Note any duplicate searches'''

import re, datetime

def transform(infilename):
    '''Copy file to file.new, filtering to enable all searches and set all creation dates. Note duplicate searches.'''
    
    cre=re.compile(r'CreateTime="([^"]*)"')
    ere=re.compile(r'Enabled="0"')
    sre=re.compile(r'Search Query="([^"]*)"')
    lre=re.compile(r'LifeTime="([^"]*)"')
    rre=re.compile(r'ReissueTimeout="([^"]*)"')
    todaysdate=datetime.date.today().isoformat()
    


    duplicates={}
   
    f=open(infilename, "r")
    of=open(infilename+".new","w")

    for line in f.readlines():
        query = sre.findall(line)
        if len(query)>0:
            duplicates[query[0]]=duplicates.get(query[0],0)+1
        line=cre.sub('CreateTime="'+todaysdate+' 00:00:00"', line)
        line=ere.sub('Enabled="1"',line)
        line=lre.sub('LifeTime="336"', line)
        line=rre.sub('ReissueTimeout="3600"', line)
        of.write(line)

    dups = [x for x in duplicates.keys() if duplicates[x]>1]
    print "Duplicate searches in "+infilename
    print dups
    
    f.close()
    of.close()


transform("searches.xml")

    



