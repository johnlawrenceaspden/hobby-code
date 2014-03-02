/* Here's the integral from today's lecture. If you play with this
little program and adapt it to do some of the other integrals that
we've talked about, it will teach you everything you need to know
about integration.*/

#include <stdio.h>

/* The integral from y=0 to 1 of (the integral from x=y to 2-y of (2x^2+y) dx) dy */

void main(void)
{
  double x,y,sum=0;
  for(y=0; y<1; y+=0.01)
    for(x=y; x<2-y; x+=0.01)
      sum+=(2*x*x+y)*0.01*0.01;
  printf("numerical approximation=%f", sum);
  printf("exact integral=%f",          8.0/3.0);
}
