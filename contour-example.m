# stolen from 
# http://msemac.redwoods.edu/~darnold/math50c/matlab/contours/index.xhtml

clear; close all;

x=linspace(-3,3,40);
y=linspace(-3,3,40);
[x,y]=meshgrid(x,y);

#z=x.^3-4*x.^2+2*x.*y-y.^2;
z=x.^4-y.^4-x.^2+4*y.^2;
z=log(z.*z);

[c,h]=contour(x,y,z);
clabel(c,h);

[c,h]=contour(x,y,z,20);
clabel(c,h);

#mesh and contours
meshc(x,y,z);

#contours on hill
contour3(x,y,z);

#implicit plotting
v=[-4,-4];
[c,h]=contour(x,y,z,v);
clabel(c,h);


#v=[-6,-5,-4,-3,-2,-1,0,1];
v=[-4,-15/4,0,1,2,3,3.9,4,4.1];
[c,h]=contour(x,y,z,v);
clabel(c,h);



grid on
axis equal
axis tight

xlabel('x-axis')
ylabel('y-axis')
title('Level curves of the function f(x,y) = x^2 + y^2.')


