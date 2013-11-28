figure; hold on

t=[-6:0.01:6]
axis([-3,3,-2,2])

plot(t, sin(t),"r")
plot(t, t)
plot(t, t-t.^3/6)
plot(t, t-t.^3/6+t.^5/120)
plot(t, t-t.^3/6+t.^5/120-t.^7/5040)
plot(t, t-t.^3/6+t.^5/120-t.^7/5040+t.^9/(5040*8*9))



plot(t,t)
plot(t, t.^3/6)
plot(t, t.^5/120)
plot(t, t.^7/5040)
plot(t, t.^9/(5040*8*9))


figure; hold on

cls

t=[-1:0.01:1]
axis([-1,1,-2,2])

plot(t, log(1+t),"r")
plot(t, t)
plot(t, t-t.^2/2)
plot(t, t-t.^2/2+t.^3/3)
plot(t, t-t.^2/2+t.^3/3-t.^4/4)
plot(t, t-t.^2/2+t.^3/3-t.^4/4+t.^5/5)



