def beats(f1,f2):
    h1={f1*x for x in range(1,10)}
    h2={f2*x for x in range(1,10)}
    all=sorted(h1|h2)
    #beats=sorted([a/b for a,b in list(zip(all,all[1:]))])
    beats=[(b-a)/b for a in all for b in all[1:]]
    beats=sorted([x for x in beats if 0.1<x<0.6])
    return beats
    
print(beats(440,660))
print(beats(440,500))
