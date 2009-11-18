import timeit

def mirror(s):
    b=len(s)-1
    f=0
    while (b>f):
        if (s[b]!=s[f]): return False
        b-=1
        f+=1
    return True

#t=timeit.Timer("""mirror("abba")""", "from __main__ import mirror")
#print t.timeit()

def secondword(s):
    return s.split()[1]

secondword("The cat jumped over the dog")


def string2integer(s):
    accum=0
    for x in s:
        accum=10*accum+int(x)
    return accum

