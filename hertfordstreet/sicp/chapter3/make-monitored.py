def make_monitored(f):
    def local(x):
        count = 0
        if(x=="count"):
            return count
        elif(x=="reset"):
            count=0
            return count
        else:
            count+=1
            return f(x)
    
    return local


from math import sqrt, sin
msqrt=make_monitored(sqrt)
msin=make_monitored(sin)
msqrt(25)
