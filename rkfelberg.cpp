// Runge-Kutta Felberg ripped off from:
// http://online.redwoods.cc.ca.us/instruct/darnold/deproj/Sp98/ERane/index.htm

#include <windows.h>
#include <math.h>

void CALLBACK rkfelberg(double cbF(double,double),void cbPlot(double,double),double y0,double a,double b,double max,double min,double tollerence)
{
//cbF is a call back to the function that will evaluate the expressions
//cbPlot is a plotting callback
//y0 is  the initial condition
//a is the min
//b is the max
//tollerence is the minimum accuracy
//returns 0 on success
//otherwise it will return an error code
   double t=a;
   double w=y0;
   double h=max;

   double k1=0.0,k2=0.0,k3=0.0,k4=0.0,k5=0.0,k6=0.0;

   double R=0.0,delta=0.0;

   while(true)
   {
		k1=h*cbF(t,w);

		k2=h*cbF(t+h/4,w+(1/4)*k1);

		k3=h*cbF(t+3*h/8,w+3*k1/32+9*k2/32);

		k4=h*cbF(t+12*h/13,w+1932*k1/2197-7200*k2/2197+7296*k3/2197);

		k5=h*cbF(t+h,w+493*k1/216-8*k2+3680*k3/513-845/4104*k4);

		k6=h*cbF(t+h/2,w-8*k1/27+2*k2-3544*k3/2565+1859*k4/4104-11*k5/40);

		R=fabs(k1/360-128*k3/4275-2197*k4/75240+k5/50+2*k6/55)/h;

		if(R<=tollerence)
		{
			t+=h;

			w+=25*k1/216+1408*k3/2565+2197*k4/4104-k5/5;

			cbPlot(t,w);
		}		
		
		delta=0.84*pow(tollerence/R,0.25);//error control the 0.25 is the usual choice, 
                                                  //however this is not always the case

		if(delta<=0.1)
			h*=0.1;
		else if(h>=4.0)
			h*=4;
		else h*=delta;

		if(h>max)h=max;

		if(t>=b)
			return;
		else if(t+h>b)
			h=b-t;
		else if(h<min)
		{
			return;
		}
   }

   return;
}
