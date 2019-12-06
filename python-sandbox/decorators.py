def double(func):
    def wrap(*args):
        return func(*func(*args))
    return wrap


@double
def foo(x):
    return (x*2,)

@double
@double
@double
def bar(x,y):
    return y,x*2


print(foo, type(foo))
print(foo(1))

print(bar(1,1))


x = [['a','a','a'],['b','b','b'],['c','c','c']]
y = zip(*x)
print(y)
print(list(y))


a = [1, 2, 3]
b = a
c = [5, 6]

a += c

print(a, b, c, sep='\n')

a = [1, 2, 3]
b = a
c = [5, 6]

a = a + c

print(a, b, c, sep='\n')

x = {1, 2, 3}
y = {2, 3, 4}

print(x | y)
print(x & y)
print(x ^ y)
print(x - y)
print(y - x)



