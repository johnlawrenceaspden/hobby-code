worddict={}

def words(filename):
    for line in open(filename):
            for word in line.split():
                yield(word)

a=words("return.txt")

def see(a, b, c):
    #print a, b, c
    val=worddict.get((a,b),[])
    val.append(c)
    worddict[(a,b)]=val

last2="."
last="."
for i in a:
    see (last2,last,i)
    last2=last
    last=i

start="."
second="."
import random

continuations=worddict.get((start,second))
while(continuations!=None):
    #if(len(continuations)>1): print "****",(len(continuations)),"****"
    next=random.choice(continuations)
    print next,
    start=second
    second=next
    continuations=worddict.get((start,second))

