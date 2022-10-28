#include<stdio.h>
#include<stdlib.h>
#include<assert.h>
#include<stdbool.h>

#define N 10

int a[N];

int display(void)
{
  bool sorted=true;
  int inversions=0;

  printf("%i",a[0]);
  for (int i=1; i<N; i++) {
    if (a[i-1]<=a[i]) printf("%i",a[i]); else {sorted=false; inversions++;  printf("*%i*",a[i]);}
  }
  if(sorted) printf("sorted"); else printf("unsorted");
  printf("(%i)\n", inversions);
  return inversions;
}

void badswap(void)
{
  int i=0,j=0;
  while(i==j){
    i=rand()%N;
    j=rand()%N;
  }
  if(i>j){  i^=j;  j^=i;  i^=j;}

  /* printf("%i,%i",i,j); */

  a[i]^=a[j];
  a[j]^=a[i];
  a[i]^=a[j];

}

void goodswap(void)
{
  int i=0,j=0;
  while(i==j){
    i=rand()%N;
    j=rand()%N;
  }
  if(i>j){  i^=j;  j^=i;  i^=j;}

  if(a[i]>a[j]){
    a[i]^=a[j];
    a[j]^=a[i];
    a[i]^=a[j];
  }
}


int main (int argc, char** argv){
  printf("Hello World!\n");
  for (int i=0; i<N; i++) a[i]=i;

  display();

  for(int i=0; i<100; i++){
    for(int i=0; i<1; i++)   badswap();
    display();
  }

  printf("-------------------------------------------\n");

  display();

  for(int i=0; i<100; i++){
    for(int i=0; i<1; i++)   goodswap();
    display();
  }

  printf("-------------------------------------------\n");

  display();

  for(int i=0; i<100; i++){
    for(int i=0; i<1; i++)   badswap();
    display();
  }

  printf("-------------------------------------------\n");

  display();

  for(int i=0; i<100; i++){
    for(int i=0; i<1; i++)   goodswap();
    display();
  }

}
