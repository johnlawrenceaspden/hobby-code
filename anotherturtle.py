import turtle

STEP = 10
SPEEDS = ['fastest', 'fast', 'normal', 'slow', 'slowest']

def hilbert(n, turn):
    if n:
        turtle.right(turn)
        hilbert(n-1, -turn)

        turtle.forward(STEP)
        turtle.left(turn)
        hilbert(n-1, turn)

        turtle.forward(STEP)
        hilbert(n-1, turn)

        turtle.left(turn)
        turtle.forward(STEP)
        hilbert(n-1, -turn)

        turtle.right(turn)

turtle.speed(SPEEDS[0])
hilbert(5, 60)
print "Done.  Press <enter> to exit."
raw_input()
