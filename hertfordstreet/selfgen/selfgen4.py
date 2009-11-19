import sys

encodedstring='''
def printinterpreted(s):
    escape=0
    for i in encodedstring:
        if(escape==0):
            if (i=='~~'):
                escape=1
                continue;
            else:
                sys.stdout.write(i),
        else:
            if(i=='n'):
                print
            if(i=='~~'):
                sys.stdout.write('~~')
            if(i=="'"):
                sys.stdout.write("'")
            escape=0
                
print "import sys"
print
print "encodedstring=~'~'~'%s~'~'~'" % encodedstring
printinterpreted(encodedstring)
'''

def printinterpreted(s):
    escape=0
    for i in encodedstring:
        if(escape==0):
            if (i=='~'):
                escape=1
                continue;
            else:
                sys.stdout.write(i),
        else:
            if(i=='n'):
                print
            if(i=='~'):
                sys.stdout.write('~')
            if(i=="'"):
                sys.stdout.write("'")
            escape=0
                
print "import sys"
print
print "encodedstring='''%s'''" % encodedstring
printinterpreted(encodedstring)
