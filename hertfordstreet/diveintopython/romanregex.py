import re

pattern='^M?M?M?M?(CM|CD|D?C?C?C?)(XC|XL|L?X?X?X?)(IX|IV|V?I?I?I?)$'

pattern2="""
^                           #beginning of string
M{0,4}                   #thousands 0 to 4 M's
(CM|CD|D?C{0,3}) #hundreds
(XC|XL|L?X{0,3})  #tens
(IX|IV|V?I{0,3})   #ones
$                           #end of string
"""

def check(str):
    print str.ljust(10),
    if re.search(pattern2, str, re.VERBOSE):
        print 'pass'
    else:
        print 'fail'

check('M')
check('MI')
check('VII')
check('MM')
check ('MMM')
check('MMMMCM')
check('MCML')
check('MCMXL')
check('MCML')
check('MCMLX')
check('MCMLXXX')
check('CMXC')

print ('\nfailure cases\n')

check('MCMLXXXX')
