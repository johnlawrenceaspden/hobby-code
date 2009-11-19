def memoize(f):
    "return a memoized version of a function"
    a={}
    def newf(*args):
        if not args in a: a[args]=f(*args)
        return a[args]
    return newf

if __name__=="__main__":
    def fib(n):
        if (n<2):
            print '!',
            return n
        else:
            print '+',
            return fib(n-1)+fib(n-2)


    print "Fibonnacci (classic)"
    print fib(10)
    
    print "Fibonacci (memoized)"
    fib=memoize(fib)
    print fib(10)
    print fib(11)

