#include<stdio.h>

#define N 1000000

int a[N];
int b[N];
int c[N];

int main(void)
{
  int i, count;
  long long sum=0;

  for (i=0; i< N; i++) {
    a[i]=i;
    b[i]=i;
  }


  for(count=0; count<100; count++){
    for (i=0; i< N; i++) {
      c[i]=a[i]+b[i];
    }


    for (i=0; i< N; i++) {
      sum+=a[i];
    }
  }

  printf("sum=%lli\n", sum);
}

/* gcc -std=gnu99 -Ofast efficiencyandprogress.c -o efficiencyandprogress && time ./efficiencyandprogress */
/* sum=49999950000000 */

/* real	0m1.735s */
/* user	0m1.668s */
/* sys	0m0.044s */

/*So summing the arrays, and reducing one of them, together take about 17ms*/


