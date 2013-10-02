/* A guest language on this blog. A welcome, please, for C */

/* The reason that I am off on this particular one at the moment is
   because I recently waited 3 hours for a clojure program to
   terminate, after about a day trying to get it to run at all. */

/* When it became apparent that it was not going to finish any time
   soon, I hacked together an answer in C, using what should have been
   a less efficient algorithm that I had come up with while
   waiting. */

/* That program took about 15 minutes to write and 65 seconds to run,
   and got the correct answer. */

/* That comparison is entirely unfair to both clojure and C in all
   sorts of ways, but if I am going to spend time getting clojure to
   run at C-ish speeds, I need to know what I should be aiming for. */

/* This program is what I am using as a comparison for (reduce + (map + _ _ )) */

/* To make sure that clever compilers and runtimes aren't doing any
   sneaky dead-code elimination, it is actually doing some sort of
   computation. But it is mainly mapping and reducing. Lots.*/

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

/* Best timing so far */

/* gcc -std=gnu99 -Ofast -march=native efficiencyandprogress.c -o efficiencyandprogress && time ./efficiencyandprogress */
/* sum=250249749750000000 */

/* real	0m8.583s */
/* user	0m8.548s */
/* sys	0m0.024s */

/* 8.6 ms to add two arrays of ints and then add one of them up */


/************************************************************************************/

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
