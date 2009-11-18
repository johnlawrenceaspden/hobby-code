import test

print test.doom()
print test.ddoom(3.0)
o=(1.0,1.7,2.3)
a=test.intArray(len(o))
for i,v in enumerate(o):
    a[i]=int(v)

print test.product(3,a)

b=test.doubleArray(len(o))
for i,v in enumerate(o):
    b[i]=v

print test.dproduct(len(o),b)



