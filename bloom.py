#filterbits=8
#filterchecksums=[2,3,7] 
#0.052

filterbits=12
filterchecksums=[2,3,5,7,11,13] 
# 0.0123

#filterbits=16
#filterchecksums=[2,3,5,7,11,13]
# 0.0027

strings=["doom","fluffy","horror","terror","greenpeace","fantastic"] 
text="""The horror of it was unbearable. it was so fluffy. She was
doomed. Doomed beyond the power of God to save her.  Bloom filters are
an old technique to save memory. Talking about bumming bits when
you're programming in python is a bit ridiculous, but what the
hey. All we need here is a list of words to test the filter
against. It doesn't matter what the words are so I'm typing stream of
consciousness. I'm sitting in CB2, where weirdly the wireless is
working after I rebooted their router for them.
Optimal checksum number for a bloom filter of size m and n elements is m/n log2
i.e around 70% of filterbits. But that's not what I'm getting here. I wonder if my checksum functions are dodgy?
"""

filtersize=filterbits*len(strings)

print "Bloom Filter, size %d" % filtersize

bf=set()

def checksum(s,p):
    cs=0
    for c in s:
        cs=p*cs+ord(c)
    return cs % filtersize

def add_to_filter(s):
    for p in filterchecksums:
        bf.add(checksum(s,p))

def check_filter(s):
    return all(checksum(s,p) in bf for p in filterchecksums)

for s in strings:
    add_to_filter(s)

print "the filter", bf

print "the things caught by the filter"
from string import punctuation

bad=0
count=0
for w in set(word.strip(punctuation).lower() for word in text.split()):
    count+=1
    if(check_filter(w)):
        print "caught",w,
        if w in strings:
            print "(good)"
        else:
            print "(BAD!!)"
            bad+=1

print "Bad:", bad, "Count:", count
print "Empirical ", float(bad)/count
print "Estimated ", pow((len(filterchecksums)*len(strings)/float(filtersize)),len(filterchecksums))
print "Estimated Filter Load", (len(filterchecksums)*len(strings)/float(filtersize))
print "Empirical Filter Load", len(bf)/float(filtersize)
print "Filter size", filtersize
print "Filter", bf

k=len(filterchecksums)
print "k=",k
m=filtersize
print "m=", m
n=len(strings)
print "n=",n 
print "m/n log2 (optimal filtersize)", (math.log(2)*m)/n
print "estimated false postives", pow(1-(pow(1-1.0/m,k*n)),k)
