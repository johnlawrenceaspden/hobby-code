import turtle

def fib(n):
    if n<3:
        turtle.color('green', 'green')
    else:
        turtle.color('brown', 'brown')
        
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
