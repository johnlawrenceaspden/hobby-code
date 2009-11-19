#!/usr/bin/env python

""" Recursively follow all links from my homepage, where a link is anything that matches linkre """

import urllib, re

invalidprotocols=[]

linkre=re.compile(r'''(
    \w+://                        #http://, ftp:// etc
    [^\\{}|[\]^<>"'\s]*       #legal url characters
    [^\\{}|[\]^<>"'\s.,;?:!]  #end in a legal url character that is not punctuation: consider www.google.com.
    )''', re.VERBOSE)

internallinkre=re.compile(r'''
    href="
    (
    [^\\{}|[\]^<>"'\s]*       #legal url characters
    [^\\{}|[\]^<>"'\s.,;?:!]  #end in a legal url character that is not punctuation: consider www.google.com.    
    )''', re.VERBOSE)



def printspiderresults(checked, tocheck, linksgraph):
    'assuming checked and tocheck are lists of urls, display them'
    print "********************"
    if len(checked)>0:
        print "Sites checked so far\n"
        print '\n'.join(checked)
    if len(tocheck)>0:
        print "\nSites to check\n"
        print '\n'.join(tocheck)
    if len(linksgraph)>0:
        print "\nLinks Graph\n"
        for k,v in linksgraph.items():
            for i in v:
                print k, "->", i
    if len(invalidprotocols)>0:
        print "\nInvalid Protocols\n"
        print invalidprotocols
    print "******************"   

def findalllinks(url, linkregexp):
    'given a url, return all occurrences of the regular expression linkregexp'
    alllinks=[]
    print "checking", url
    for i in urllib.urlopen(url).readlines():
        matches=re.search(linkregexp,i)
        if matches :
            print i
            print matches.groups()
            alllinks.extend(matches.groups())
        matches=re.search(internallinkre,i)
        if matches:
            print i
            print matches.groups()
    return alllinks

def validprotocol(url):
    if url.find('http://')==0 : return True
    if url.find('https://')==0 :
        return True
    else:
        print 'invalid protocol', url
        invalidprotocols.append(url)
        return False

if __name__=='__main__':
    sitestocheck=[]
    siteschecked=[]
    linksgraph={}
    sitestocheck.append("http://www.aspden.com")
    printspiderresults(siteschecked, sitestocheck, linksgraph)
    
    while(len(sitestocheck)>0):
        sitetocheck=sitestocheck.pop()
        if sitetocheck in siteschecked:
            print "cycle detected:", sitetocheck
            continue
        if not validprotocol(sitetocheck):
            continue
            

        links=findalllinks(sitetocheck, linkre)

        if len(links)>0:
            print "found in ", sitetocheck
            print links
        siteschecked.append(sitetocheck)
        sitestocheck.extend(links)
        a=linksgraph.get(sitetocheck, [])
        a.extend(links)
        linksgraph[sitetocheck]=a
        printspiderresults(siteschecked, sitestocheck, linksgraph)
