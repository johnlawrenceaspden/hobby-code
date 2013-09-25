#include<stdio.h>

#define N 1000000

int a[N];
int b[N];

int main(void)
{
  int i, count;
  long long sum=0;

  for (i=0; i< N; i++) {
    a[i]=i;
  }

  for(count=0; count<1000; count++){
    for (i=0; i< N; i++) {
      b[i]+=a[i];
    }

    for (i=0; i< N; i++) {
      sum+=b[i];
    }
  }

  printf("sum=%lli\n", sum);
}

/* gcc -std=gnu99 -Ofast efficiencyandprogress.c -o efficiencyandprogress && time ./efficiencyandprogress */
/* sum=250249749750000000 */

/* real	0m16.053s */
/* user	0m15.992s */
/* sys	0m0.032s */

/* So it looks as though adding one array to another and then adding up all the values in an array takes about 16ms in total.*/
/* That's 16ns per array entry which looks sort of OK on my little netbook, which boast an Intel Atom CPU N455 running at 1.66GHz with a 512kb*/

/* I'm hoping there's enough complexity here that the compiler actually has to run the program rather than taking short cuts*/

/* But just as a check, here's the code running with gcc in 'do exactly what I say so I can debug it' mode.*/

/* gcc -std=gnu99 -O0 efficiencyandprogress.c -o efficiencyandprogress && time ./efficiencyandprogress */
/* sum=250249749750000000 */

/* real	0m27.850s */
/* user	0m27.692s */
/* sys	0m0.060s */

/* This produces a small constant factor speedup, as expected if the two versions are doing the same work. */
