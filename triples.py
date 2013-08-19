# [(x,y,z) | x<- [1..100], y<-[1..100], z<-[1..100], x<y, x*x+y*y==z*z, (gcd (gcd x y ) z)==1]
# let gcd a b = (if a==0 then b else (if a<b then (gcd b a) else (gcd (a - b) b)))

def gcd(a,b):
    if a==0: 
        return b 
    else: 
        if a<b:
            return gcd(b,a)
        else:
            return gcd(a-b,b)

print [(x,y,z) for x in range(1,100) for y in range(1,100) for z in range (1,100) if x<y if x*x+y*y==z*z if gcd(gcd(x,y),z)==1]
    
