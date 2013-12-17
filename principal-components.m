X=[1, 2, 4;
   3, 4, 5;
   5, 6, 8;
   7, 10, 5];

hold off
plot(X')

sigma=X*X'

[U,S,V]=svd(sigma)

sum(diag(S))

sum(diag(S)(1))/sum(diag(S)) # 99.7% of variance retained
sum(diag(S)(1:2))/sum(diag(S)) #100% of variance retained
sum(diag(S)(1:3))/sum(diag(S)) #100% of variance retained


Ureduce1=U(:,1:1) #Just the first component, then
Ureduce2=U(:,1:2) #Or the two important ones
Ureduce3=U(:,1:3)

# the compressed representation
Z1=Ureduce1'*X
Z2=Ureduce2'*X
Z3=Ureduce3'*X

# and uncompress it again
Xapprox1=Ureduce1*Z1
Xapprox2=Ureduce2*Z2
Xapprox3=Ureduce3*Z3

figure
plot(X')
hold on
plot(Xapprox1')
plot(Xapprox2')
plot(Xapprox3')


## should really remove DC and scale first!

N=X-mean(X)

sigma2=N*N'
[U2,S2,V2]=svd(sigma2)
