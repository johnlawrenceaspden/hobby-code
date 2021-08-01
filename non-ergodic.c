#include<stdio.h>
#include<stdlib.h>
#include<time.h>

void main(void){
  double a=1000;
  int wins=0;
  int losses=0;
  printf("hello\n");
  srand((unsigned int)time(NULL));
  for(int i=0; i<100; i++){
    if(rand()%2 == 0){
      printf("win");
      wins++;
      a*=1.5;
    } else {
      losses++;
      printf("lose");
      a*=0.6;
    }
    printf("amount %f (%d,%d)\n",a,wins,losses);
  }
}
