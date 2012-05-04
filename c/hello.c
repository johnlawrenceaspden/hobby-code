#include<stdio.h>

typedef unsigned int uint32_t;

static unsigned long long get_verilog_time()
{
  int lo, hi;
  lo = 0x123456789;
  hi = 0xabcdef0123;

  unsigned long long rv=((unsigned long long)(uint32_t) hi) << 32 | (uint32_t) lo;

  printf("%i,%i,%i\n", sizeof(uint32_t), sizeof(unsigned long long), sizeof(int));
  printf("hi: 0x%x\n", hi);
  printf("lo: 0x%x\n", lo);
  printf("rv: 0x%llx\n", rv);
  printf("rv: 0x%llu\n", rv);

  return rv;
}

int main(void) {

/* #ifdef DOOM */
/*   printf("doom"); */
/* #else */
/*   printf("no doom"); */
/* #endif */

  get_verilog_time();

  return 0;
}
