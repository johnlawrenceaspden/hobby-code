#include<stdio.h>

#define K 15

int opt[K+1];

#define VALS 4
int size [VALS] = {5, 6, 7, 8};
int value[VALS] = {10,15,14,20};

int main(void)
{
  for(int i=0; i<VALS; i++){
    printf("item %i:",i);
    for(int j=K; j>=0; j--){
      printf("%i",j);
      if(j>=size[i]){
        int poss=opt[j-size[i]]+value[i];
        if(poss>opt[j]) opt[j]=poss;
      }
    }
    printf("\n",i);
  }
    
  printf ("knapsack size %i\n",K);
  printf ("maximal value %i\n",opt[K]);
  printf ("opt=");
  for(int i=0; i<K; i++) printf("%i,", opt[i]);
  printf("%i\n", opt[K]);
  return 0;
}
