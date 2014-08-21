# initial position x, v, a
x0=[0;0;1];

# acceleration modifies velocity, velocity modifies position
A=[0 1 0;
   0 0 1;
   0 0 0];

h=0.01;

x=[];

x(:,1)=x0;

IhA=eye(3)+h*A;

for i=1:100
  x(:,i+1)=IhA*x(:,i);
endfor


figure
axis([-1 1 -1 1 0 1])
hold on

for ii=1:100
    plot3 (x(1,ii),x(2,ii),x(3,ii),'*');
    pause (.001)
end
