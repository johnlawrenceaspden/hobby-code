% smoothing function

function A=fivepointaverage(Y)

%Y=[1,2,3,3,5,8,7,8,9,10];

%[3,3,5,8,7,8,9,10, -, -]
%[2,3,3,5,8,7,8,9 ,10, -]
%[1,2,3,3,5,8,7,8 ,9, 10]
%[-,1,2,3,3,5,8,7 ,8,  9]
%[-,-,1,2,3,3,5,8 ,7,  8]

Y1=[Y(3:end),0,0];
Y2=[Y(2:end),0];
Y3=[Y(1:end)];
Y4=[0,Y(1:end-1)];
Y5=[0,0,Y(1:end-2)];

%[-,-,5,8,7,8,9,10, -, -]
%[-,3,3,5,8,7,8,9 ,10, -]
%[1,2,3,3,5,8,7,8 ,9, 10]
%[-,1,2,3,3,5,8,7 ,8,  -]
%[-,-,1,2,3,3,5,8 ,-,  -]

Y1(1:2)=0;
Y2(1)=0;
Y4(end)=0;
Y5(end-1:end)=0;

% [1,6,14,21,26,31,37,42,27,10]

Ya=Y1+Y2+Y3+Y4+Y5;

% [1,3,5,5,5,5,5,5,3,1]

factor=5*ones(size(Y));
factor(1)=1;
factor(2)=3;
factor(end-1)=3;
factor(end)=1;


A=Ya./factor;


%plot([Y',Ys'])