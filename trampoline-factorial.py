thunk = lambda name, *args: lambda: name(*args)

#print ( thunk( lambda x,y: x*y , 2 ,3)() )

def _trampoline(bouncer):
    while callable(bouncer):
        bouncer = bouncer()
    return bouncer

# print ( _trampoline( thunk ( lambda x,y: x*y, 2 ,3)))

trampoline = lambda f: lambda *args: _trampoline(f(*args))

identity = lambda x: x

_factorial = lambda n, c=identity: c(1) if n == 0 else thunk(_factorial, n - 1, lambda result: thunk(c, n * result))

factorial = trampoline(_factorial)
