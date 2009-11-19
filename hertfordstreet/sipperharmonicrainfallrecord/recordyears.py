def incrementsequence(seq,max):
    for i,x in enumerate(seq):
        if (x<max):
            seq[i]+=1
            break
        else:
            seq[i]=1

def possiblerolls(die,N):
    rolls=[1]*N
    for i in range(die**N):
        yield(rolls)
        incrementsequence(rolls,die)


def no_of_records(seq):
    record=seq[0]
    count=1
    for i in (seq[1:]):
        if i>record:
            count += 1
            record=i
    return count

def proportions(recorddistribution):
    total=sum(recorddistribution.values())
    for i in recorddistribution:
        recorddistribution[i]=float(recorddistribution[i])/total
    return recorddistribution

def expectation(pd):
    exp=0.0
    for (k,v) in pd.items():
        exp+=k*v
    return exp
    
recorddistribution={}
for n in [1,2,4,8,16,32]:
    recorddistribution={}
    for s in possiblerolls(n,4):
        recs=no_of_records(s)
        recorddistribution[recs]=recorddistribution.get(recs,0)+1

    print n
    print recorddistribution
    recordprobability = proportions(recorddistribution)
    print recordprobability
    print expectation(recordprobability)



