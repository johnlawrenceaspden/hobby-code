#include<stdio.h>
#include<stdlib.h>
#include<inttypes.h>
#include<assert.h>

/**

   https://stackoverflow.com/questions/44166714/in-c-how-do-i-calculate-the-signed-difference-between-two-48-bit-unsigned-integ 

   I've got two values from an unsigned 48bit nanosecond counter, which may wrap.

   I need the difference, in nanoseconds, of the two times.

   I think I can assume that the readings were taken at roughly the same time, so of the two possible answers I think I'm safe taking the smallest.

   They're both stored as uint64_t. Because I don't think I can have 48 bit types.

   I'd like to calculate the difference between them, as a signed integer (presumably int64_t), accounting for the wrapping.

   and so on.....

*/

/** 

    To examine the produced assembler 
    gcc -std=c99 -O3 -o 48bitmath 48bitmath.c
    objdump -d 48bitmath
    objdump -d 48bitmath | grep -C10 sub48 

*/

  
/* Use noinline to stop gcc optimising the crap out of the various test functions */
 int64_t __attribute__((noinline)) sub48a(uint64_t x, uint64_t y)
 {
   uint64_t diff;
  
   diff=x-y;

   int64_t shifteddiff;
   shifteddiff=(diff << 16);
  
   int64_t sdiff;
   sdiff= shifteddiff >> 16;

   /* printf("x=%"PRIu64" y=%"PRIu64"\n",x,y); */

   /* printf("x=%"PRIu64"(0x%"PRIx64")\n",x,x); */
   /* printf("y=%"PRIu64"(0x%"PRIx64")\n",y,y); */
  
   /* printf("diff=%"PRIu64"(0x%"PRIx64")\n",diff,diff); */

   /* printf("shifteddiff=%"PRId64"(0x%"PRIx64")\n",shifteddiff,shifteddiff); */

   /* printf("sdiff=%"PRId64"(0x%"PRIx64")\n",sdiff,sdiff); */

   /* printf("\n"); */

   return sdiff;
  
 }


int64_t __attribute__((noinline)) sub48b(uint64_t x, uint64_t y)
{

  x<<=16;
  y<<=16;
  
  int64_t diff;
  
  diff=x-y;

  diff>>=16;

  return diff;
  
}


int64_t __attribute__((noinline)) sub48c(uint64_t x, uint64_t y)
{
  return ((int64_t)((x<<16)-(y<<16)))>>16;
}


int64_t __attribute__((noinline)) sub48d(uint64_t x, uint64_t y)
{
  return ((int64_t)((x-y)<<16)>>16);
}



int main(int argc, char**argv){


  int64_t (*sub48)(uint64_t x, uint64_t y);

  int64_t (*sub48s[])(uint64_t x, uint64_t y)={&sub48a, &sub48b, &sub48c, &sub48d};

  int arraylen=sizeof(sub48s)/sizeof(int64_t (*)(uint64_t x, uint64_t y)); 

  for(int i=0; i<arraylen;i++)
    { sub48=sub48s[i];
      uint64_t x;
      uint64_t y;

      x=5; y=3;
      assert((*sub48)(x,y)==2);

      x=3; y=5;
      assert((*sub48)(x,y)==-2);

      x=0xffffffffffff;
      y=0x000000000000;
      assert((*sub48)(x,y)==-1);
      assert((*sub48)(y,x)==+1);

      x=0xffffffffffff;
      y=0x000000000002;
      assert((*sub48)(x,y)==-3);
      assert((*sub48)(y,x)==+3);

      x=0xfffffffffff8;
      y=0xfffffffffff9;
      assert((*sub48)(x,y)==-1);
      assert((*sub48)(y,x)==+1);

      x=0x7fffffffffff;
      y=0x800000000002;
      assert((*sub48)(x,y)==-3);
      assert((*sub48)(y,x)==+3);

      x=0xfffffffffffff;
      y=0xf000000000000;
      assert((*sub48)(x,y)==-1);
      assert((*sub48)(y,x)==+1);

      /* this bit means that gcc can't precalculate all the results and actually has to produce a function */
      srand(argc); //but it's still deterministic, provide different numbers of args to get different results
      printf("%"PRIx64"\n",(*sub48)((((uint64_t)rand()<<32)+rand()),((uint64_t)rand()<<32)+rand()));

    }
  return 0;
}
