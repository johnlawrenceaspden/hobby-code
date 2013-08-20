#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
 
int main(void)
{
  int N=50000;
  int thin=1000;
  int i,j;
  gsl_rng *r = gsl_rng_alloc(gsl_rng_mt19937);
  double x=0;
  double y=0;
  printf("Iter x y\n");
  for (i=0;i<N;i++) {
    for (j=0;j<thin;j++) {
      x=gsl_ran_gamma(r,3.0,1.0/(y*y+4));
      y=1.0/(x+1)+gsl_ran_gaussian(r,1.0/sqrt(2*x+2));
    }
    printf("%d %f %f\n",i,x,y);
  }
}

// gcc -O4 gibbs.c -lgsl -lgslcblas -lm -o gibbs
// time ./gibbs > delete.me

// gcc gibbs.c -lgsl -lgslcblas -lm -o gibbs
// time ./gibbs > delete.me
// real	1m35.539s
// user	1m34.648s
// sys	0m0.068s
