#!/usr/bin/env python2.4

import sys
import random
from optparse import OptionParser



def printpagefulofrolls(die,columns,lines):
    lines-=1    #leave room for the title
    columns-=1  #leave a slight border to remove the nagging doubt that 1 may be 19 truncated

    print "D",die

    for l in range(lines):
        c=0
        linestr=""
        while(1):
            throw="%s" % random.randint(1,die)
            if c+len(throw)>columns:
                break;
            linestr+=throw
            c+=len(throw)
            if c+1>columns:
                break
            linestr+=" "
            c+=1
            
        print linestr
    

def main():

    parser=OptionParser(usage="%prog [options] die\n      prints a page of die rolls")
    parser.add_option("-c", "--columns", type=int, help="columns on page", default=80)
    parser.add_option("-l", "--lines", type=int, help="lines in page", default=66)
    (options,args)=parser.parse_args()
    if len(args)!=1:
        parser.error("must specify how many sides for the die")
    if not args[0].isdigit():
        parser.error("argument must be an integer")

    printpagefulofrolls(int(args[0]), options.columns, options.lines)

           

if __name__ == "__main__":
    main()




    
