a=0
b=-7/3
c=5/3
print(a,b,c)

import random
f=random.random
x=f()
y=f()
z=f()

print (x,y,z)

ax=a*x+2*z
ay=(b+1)*y+(c-1)*z
az=(c+1)*y+(b-1)*z

print (ax,ay,az)

print (ax+2*ay+az)
