// gcc -O3 euler.c -lrt && ./a.out


#include<stdio.h>
#include<time.h>

void main (void){

  struct timespec start, finish;

  double cpuspeed = 1.662;
  long i,its = 10000000;
  double t=0;
  double y=0;
  double h=1.0/its;
  
  clock_gettime(CLOCK_REALTIME, &start);
  for(i = 0; i < its; i++){
    y = y+h*(t-y);
    t = t+h;
  }
  clock_gettime(CLOCK_REALTIME, &finish);
  
  printf("y=%f t=%f\n", y,t); 
  printf("start: %d %d finish: %d %d\n", start.tv_sec, start.tv_nsec, finish.tv_sec, finish.tv_nsec);
  printf("cycles/iteration: %f\n",  ((cpuspeed * ((1000000000.0 * (finish.tv_sec - start.tv_sec)) + (finish.tv_nsec - start.tv_nsec))) / its));
}

