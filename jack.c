#include<stdio.h>

double average(double a, double b)
{
  return (a+b)/2;

}

double improve(double x)
{
  return (average (x, 11/x));
}


double absolute(double x)
{
  if (x<0)
    return -x;
  else
    return x ;
}



int main(void)
{
  double c;
  double g;
  double x;
  printf("hello world!\n");
  c=average(5,2);
  printf("c=%f\n",c);
  c=absolute(5);
  printf("c=%f\n",c);
  c=absolute(-5);
  printf("c=%f\n",c);
  printf("------\n");
  x=1.0;
  printf("x=%f\n",x);
  x=improve(x);
  printf("x=%f\n",x);
  x=improve(x);
  printf("x=%f\n",x);
  x=improve(x);
  printf("x=%f\n",x);
  x=improve(x);
  printf("x=%f\n",x);
  x=improve(x);
  printf("x=%f\n",x);
  x=improve(x);
  printf("x=%f\n",x);
  x=improve(x);
  printf("x=%f\n",x);
  x=improve(x);
  printf("x=%f\n",x);
}
