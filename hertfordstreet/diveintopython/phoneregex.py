import re

phonePattern = re.compile('''
(\D+|^)                 #any number of non-digits
(\d{3})             #area code, group of 3 digits
\D*                  #non-digits
(\d{3})             #group of three
\D*                  #non-digits
(\d{4})             #group of four digits
(\D+|$)                  #non-digits
(\d*)                #extension number if present
$                     #end of line
''', re.VERBOSE)

def check(str):
    print str.ljust(10),
    groups=re.search(phonePattern, str)
    if groups:
        print 'pass',
        print groups.groups()
    else:
        print 'fail'

phonenos=[
    '800-555-1212',
    '800 555 1212',
    '800.555.1212',
    '(800) 555-1212',
    '1-800-555-1212',
    '800-555-1212-1234',
    '800-555-1212x1234',
    '800-555-1212 ext. 1234',
    'work 1-(800) 555.1212 #1234',
    '(800)5551212 x1234',
    '8005551212x1234'
    ]

dudphonenos=[
    '01223 526562',
    'ababah',
    '234*456*234x234',
    '1234-345-4567',
    '123-234-23456'
    ]



for phoneno in phonenos:
    check(phoneno)

print ('\nfailure cases\n')

for phoneno in dudphonenos:
    check(phoneno)
