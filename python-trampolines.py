# A simple demo of the trampoline technique, which enables
# tail-recursive functions to run in constant space.
#
# The mechanism is exposed for ease of understanding, rather
# clothed in magic for ease of use.
#
# (C) George Kangas, 4/23/2009.  Use any way you want.

# Here's a trampoline "client".
# Usage: trampoline(factorial, 10) ==> 3268800.
def factorial(n, p = 1):
    if n < 2:
        # If the computation is complete, return (False, result).
        return (False, p)
    else:
        # If not complete, return (True, thunk), such that
        # "thunk()" will be the required tail call.
        # To avoid cheating on the constant space spec, we
        # must package intermediate results as default args.
        return (True, lambda n=n, p=p: factorial(n-1, n*p))

# Here's the trampoline "server".
def trampoline(fun, *args):
    # Initialize the "running" flag and a thunk.
    running = True
    thunkOrResult = lambda : fun(*args)
    while running:
        # Call the thunk, to get new flag and new thunk (or result).
        (running, thunkOrResult) = thunkOrResult()
    # On exit, we have a result.
    return thunkOrResult

# More clients, all ready to play on the trampoline.

def fibonacci(n, a = 0, b = 1):
    if n == 0:
        return (False, a)
    else:
        return (True, lambda n=n, a=a, b=b: fibonacci(n-1, b, a+b))

def add(a, b):
    if a == 0:
        return (False, b)
    else:
        return (True, lambda a=a, b=b: add(a-1, b+1))
    
# In "factorial", 1,...,n are "factors", i.e. they're multiplied;
# in "termial", 0,...,n are "terms", i.e. they're added.
# So, there's as much recursion as factorial, with much smaller
# results: termial(n) == n*(n+1)/2.
def termial(n, s = 0):
    if n == 0:
        return (False, s)
    else:
        return (True, lambda n=n, s=s: termial(n-1, n+s))

# A couple of mutual tail callers.
def even(n):
    if n == 0:
        return (False, True)
    else:
        return (True, lambda n=n: odd(n-1))

def odd(n):
    if n == 0:
        return (False, False)
    else:
        return (True, lambda n=n: even(n-1))


