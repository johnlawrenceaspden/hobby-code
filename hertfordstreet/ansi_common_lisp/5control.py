def precedes(a, l):
    acc=[]
    for i in range(1, len(l)):
            if (l[i]==a):
                    if (not l[i-1] in acc):
                        acc.append(l[i-1])
    return acc
            
print precedes('a',"abracadabra")
