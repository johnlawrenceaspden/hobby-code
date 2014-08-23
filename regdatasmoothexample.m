# octave packages seem to conflict and the function ddmat
# somehow doesn't exist. Unloading everything and adding just the data-smoothing package fixes this
pkg unload all
pkg load data-smoothing

 npts = 100;
 x = linspace(0,2*pi,npts)';
 x = x + 2*pi/npts*(rand(npts,1)-0.5);
 y = sin(x);
 y = y + 1e-1*randn(npts,1);
 yp = ddmat(x,1)*y;
 y2p = ddmat(x,2)*y;
 [yh, lambda] = regdatasmooth (x, y, "d",4,"stdev",1e-1,"midpointrule");
 lambda
 yhp = ddmat(x,1)*yh;  
 yh2p = ddmat(x,2)*yh;
 clf
 subplot(221)
 plot(x,y,'o','markersize',5,x,yh,x,sin(x))
 title("y(x)")
 legend("noisy","smoothed","sin(x)","location","northeast");
 subplot(222)
 plot(x(1:end-1),[yp,yhp,cos(x(1:end-1))])
 axis([min(x),max(x),min(yhp)-abs(min(yhp)),max(yhp)*2])
 title("y'(x)")
 legend("noisy","smoothed","cos(x)","location","southeast");
 subplot(223)
 plot(x(2:end-1),[y2p,yh2p,-sin(x(2:end-1))])
 axis([min(x),max(x),min(yh2p)-abs(min(yh2p)),max(yh2p)*2])
 title("y''(x)")
 legend("noisy","smoothed","-sin(x)","location","southeast");
 %--------------------------------------------------------
 % smoothing of monotonic data, using "stdev" to determine the optimal lambda
