#include<stdio.h>

/*(reduce + (range 1 11))*/

int list[]={1,2,3,4,5,6,7,8,9,10};
int len=10;

int a=0;
int i;

void main (void){
  for (i=0; i<len; i++){
    a += list[i];
  }
  printf ("%d\n", a);
}
