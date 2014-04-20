lb2kg={}

for lb in range(10):
    for oz in range (16):
        kg=(16*lb+oz)/16./2.2046
        lb2kg[(lb,oz)]=kg
        #print "%2d %2d %.3f" % (lb, oz, kg)


skeys=sorted(lb2kg.keys())

count=len(skeys)
span=6
stride=count/span

print "lb oz -> kg.gram | 1 lb 0 oz 0.455kg | 1 pound is 455 grams"
print

for i in range(stride):
    dlist=[]
    for s in range(span):
        (lb,oz)=skeys[i+s*stride]
        kg=lb2kg[(lb,oz)]
        dlist.append( "%2d %2d %.3f" % (lb, oz, kg))
    print " |".join(dlist)

