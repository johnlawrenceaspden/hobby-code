#include <stdio.h>
#include <stdlib.h>


double sqrt(double p)
{
  double guess=1;
  for(int i =0; i<10; i++)
    {
      guess=(p/guess+guess)/2;
    }
  return p;
}

double exp(double r)
{
  return (1+r*(1+r/2*(1+r/3*(1+r/4*(1+r/5*(1+r/6*(1+r/7*(1+r/8*(1+r/9)))))))));
}

double ansa(int n, double a)
{
    double x,y,z,sum=0;
    for(int i=0; i<a*a*n; i++)
    {
        x=a*drand48();
        y=a*drand48();
        double r= x*x+y*y;
        sum+=1/exp(r);
    }
    return 4*sum/n;
}

double ansa2(int n, double a)
{
    double x,y,z,sum=0;
    for(int i=0; i<a*n; i++)
    {
        x=a*drand48();
        double r= x*x;
        sum+=1/exp(r);
    }
    return 2*sum/n;
}

int main()
{
    printf("Gaussian Integral!\n");
    for(int i=0; i<2; i++)
      printf("answer=%f\n", ansa(100000,9));
    for(int i=0; i<2; i++)
      printf("answer=%f\n", ansa2(100000,9));
    printf("answer=%f\n", sqrt(3.14159265358));
    return 0;
}
