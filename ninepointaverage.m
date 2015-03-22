% smoothing function

function A=ninepointaverage(Y)

Ym4=[Y(5:end),0,0,0,0];
Ym3=[Y(4:end),0,0,0];
Ym2=[Y(3:end),0,0];
Ym1=[Y(2:end),0];
Y0=[Y(1:end)];
Yp1=[0,Y(1:end-1)];
Yp2=[0,0,Y(1:end-2)];
Yp3=[0,0,0,Y(1:end-3)];
Yp4=[0,0,0,0,Y(1:end-4)];


Ym4(1:4)=0;
Ym3(1:3)=0;
Ym2(1:2)=0;
Ym1(1)=0;
Yp1(end)=0;
Yp2(end-1:end)=0;
Yp3(end-2:end)=0;
Yp4(end-3:end)=0;

% [1,6,14,21,26,31,37,42,27,10]

Ya=Ym4+Ym3+Ym2+Ym1+Y0+Yp1+Yp2+Yp3+Yp4;

% [1,3,5,5,5,5,5,5,3,1]

factor=9*ones(size(Y));
factor(1)=1;
factor(2)=3;
factor(3)=5;
factor(4)=7;
factor(end-3)=7;
factor(end-2)=5;
factor(end-1)=3;
factor(end)=1;


A=Ya./factor;


