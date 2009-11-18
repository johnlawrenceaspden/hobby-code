import turtle

"""
def fib(i):
    if i <= 1:
        return 1
    else:
        return fib(i-1) + fib(i-2)

n = 0
while n < 30:
    turtle.forward(fib(n))
    turtle.left(90)
    n += 1
"""

def fib(n):
    turtle.forward(30)

    if n<2:
        pass
    else:
        turtle.left(15)
        fib(n-1)
        turtle.right(30)
        fib(n-2)
        turtle.left(15)

    turtle.forward(-30)


turtle.reset()
fib(10)
