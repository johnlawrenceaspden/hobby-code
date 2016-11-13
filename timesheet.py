#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Timesheet Interpreter: edit in python-mode for the program, csv-mode for the string which is parsed.

# All lines of the string will be ignored,
# except those that contain strings of form dd:dd-dd:dd, which will be treated as chargeable intervals.
# in these cases, the first double-quoted string on the line will be treated as the project code.


#verifieddict={"companyid":(mins,rate)}
verifieddict={"acompany":(315,70), "bcompany":(360,80)}

#  Add new work and invoices at the top
#  example lines
#  INVOICE #I137 3.75 hours "acompany"
#  mon 13th  : "bcompany"  22:10-00:40 getting tests to behave, stability finally achieved
timesheet=r'''
Client: Acompany
Buzzwords: C, python, clojure

Client: Bcompany
Buzzwords: R, matlab

todo

November 2016

INVOICE #I2000 4 hours "bcompany"

tue 14th : "bcompany" 09:00-12:00 14:00-17:00  6 hours of corporate dronery

mon 13th : "acompany" 10:00-15:15   5 1/4 hours of bureaucracy, being shown fire exits


'''

import re
from datetime import datetime

if (__name__ == "__main__"):
    tdict={}
    idict={}
    pdict={}
    for this_line in reversed(timesheet.splitlines()):
        print this_line
        
        #find everything that looks like 17:45-23:00, and everything that doesn't
        time_intervals = re.findall(r'(\d\d):(\d\d)-(\d\d):(\d\d)', this_line)
        other_things   = " ".join(re.split(r'\d\d:\d\d-\d\d:\d\d', this_line))

        # inspect the remains for things like 3:4, which are probably typos which should be in some interval
        times=re.findall(r'(\d+:\d+)', other_things)
        assert not times, "times not part of interval"+str(times)

        # if there are intervals
        if time_intervals :
            # use the first quoted string on the line as a project code
            words = re.findall(r'\"([^\"]*)\"',this_line)
            assert (len(words) >= 1), "put a project code for each line"
            project_code=words[0]
            # and bill all intervals to that
            for (a,b,c,d) in time_intervals:
                print a+":"+b,c+":"+d, project_code

                t1=60*int(a)+int(b)
                t2=60*int(c)+int(d)
                tdiff=t2-t1
                if (tdiff < 0): # what about 23:15-00:20 ?
                    tdiff+=24*60

                tdict[project_code]=tdict.get(project_code, 0)+tdiff
                print project_code,tdiff, tdict

        invoice = re.findall(r'INVOICE (#I\d+) ([\d\.]+) hours "(\w+)"',this_line)
        if invoice :
            print invoice
            (invoice_number, hours, project_code) = invoice[0]
            _,rate=verifieddict[project_code]
            print (invoice_number, hours, project_code, rate)
            charge=float(hours)*rate
            print invoice_number," ", hours, " hours £", charge,"  VAT £", 0.20*charge, " TOTAL £",1.20*charge
            idict[project_code]=idict.get(project_code,0)+float(hours)

        paid = re.findall(r'PAID (#I\d+) £([\d\.]+) "(\w+)"',this_line)
        if paid :
            print paid
            (invoice_number, cash, project_code) = paid[0]
            print (invoice_number, cash, project_code)
            pdict[project_code]=pdict.get(project_code,0)+float(cash) 

verifiedkeys=set(verifieddict.keys())

allkeys=set.union(set(idict.keys()), set(tdict.keys()))

if (verifiedkeys==allkeys):

    print "WORKED"
    for p in tdict.keys():
        _,rate=verifieddict[p]
        print p, tdict[p],"minutes", tdict[p]/60.0, "hours" "  £",tdict[p]/60.0*rate 

    print "INVOICED"
    for p in idict.keys():
        _,rate=verifieddict[p]
        print p, idict[p], "hours", "  £",idict[p]*rate 


    print "PAID"
    for p in pdict.keys():
        _,rate=verifieddict[p]
        print p, "£",pdict[p], float(pdict[p])/float(rate)

    print "UNINVOICED"
    for p in allkeys:
        _,rate=verifieddict[p]
        diff=tdict.get(p,0)-idict.get(p,0)*60
        print p, diff, "minutes", diff/60.0, "hours", "  £",diff/60.0*rate, "VAT £", 0.20*diff/60.0*rate, "TOTAL £", 1.20*diff/60.0*rate, "\n"

    print "-------------"

    for project in verifieddict:
        verifiedtime,rate=verifieddict[project]
        calculatedtime=tdict[project]
        if( calculatedtime != verifiedtime ):
            print project, (calculatedtime-verifiedtime), "minutes (",(calculatedtime-verifiedtime)/60., "hours) more than when last updated"    
        else:
            print project,verifiedtime,"ok"
            
else:
    
    print "\n\nERROR: job code unaccounted for", verifiedkeys," vs ", allkeys

