#!/usr/bin/octave --persist

#piecewise linear function, a is 1 between 1 and 2, 2 between 2 and 5, etc
a=[1,2,3,4]
x1=[1,2,5,7]
x2=[2,5,7,8]

#plot looks like:
plot([1,2,2,5,5,7,7,8],[1,1,2,2,3,3,4,4])

# can do this like:
plot(reshape([x1;x2],1,8),reshape([a;a],1,8))

# or even better, like:
plot(reshape([x1;x2],1,[]),reshape([a;a],1,[]))

# if they're column vectors, then just transpose first
figure()
a=[1,2,3,4]'
x1=[1,2,5,7]'
x2=[2,5,7,8]'
plot(reshape([x1';x2'],1,[]),reshape([a';a'],1,[]))
