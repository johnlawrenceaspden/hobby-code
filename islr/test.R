#!/usr/bin/r 
cat("hello\n")

## An Introduction to Statistical Learning (with applications in R)
## http://www-bcf.usc.edu/~gareth/ISL/
           
#install.packages('ISLR')

library(ISLR)

data(package="ISLR")

x=rnorm(50)
y=x+rnorm(50, mean=50, sd=0.1)

cor(x,y)

hist(x)
plot(x)
plot(x,y,xlab='X',main='doom')

sqrt(var(rnorm(1000000)))
sd(rnorm(1000000))
mean(rnorm(1000000))

pdf("test.pdf")
plot(x,y,xlab='X',main='doom')
dev.off()


x = seq ( - pi , pi , length =50)
y=x

f=outer(x,y,function(x,y)cos(x)/(1+y^2))

contour(x,y,f)

contour(x,y,f,nlevels=45)

contour(x,y,f,nlevels=45,add=T)

fa =( f - t ( f ) ) /2

contour (x ,y , fa , nlevels =15)
contour(x,y,f,nlevels=45,add=T,col='red')

image(x,y,fa)
contour(x,y,fa,nlevels=15,add=T)

persp(x,y,fa)

persp(x,y,fa,theta=30,phi=30)


A = matrix (1:16 ,4 ,4)

A

A[2,3]

A[c(2,3),c(2,3)]

A[-c(2,3),]

dim(A)

data(Auto)

fix(Auto)

plot(Auto$cylinders,Auto$mpg)

ls()

attach(Auto)

ls()

plot(cylinders,mpg)

cylinders=as.factor(cylinders)

plot(cylinders,mpg)

plot(cylinders,mpg,varwidth=TRUE)

hist(mpg, breaks=5)

hist(mpg, breaks=15, add=TRUE)

hist(mpg, varwidth=TRUE)

pairs(Auto)

pairs(~mpg+displacement+horsepower+weight,Auto)

cor(Auto$mpg,Auto$displacement)

cor(1/Auto$mpg,Auto$displacement)

plot(1/Auto$mpg,Auto$displacement)

plot(horsepower,mpg)
identify(horsepower,mpg,name)

summary(Auto)

str(Auto)

