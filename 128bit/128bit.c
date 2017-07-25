#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

typedef __int128_t int128 ;
typedef __uint128_t uint128 ;

typedef __int64_t int64 ;
typedef __uint64_t uint64 ;


void print_uint128_dec(uint128 n)
{
    if (n == 0) {
      return;
    }

    print_uint128_dec(n/10);
    putchar(n%10+0x30);
}

void print_uint128_hex(uint128 n)
{
    if (n == 0) {
      printf("0");
      return;
    }

    print_uint128_hex(n/16);
    printf("%x",n%16);
}


int main(void)
{

  uint128  x = (((uint128) 0x01ffffffffffffff) << 64) + (uint128) 0xffffffffffffffff;

  print_uint128_dec(x); puts("");
  print_uint128_hex(x); puts("");
  // printf("__int128 max  %016" PRIx128 "\n",x);

  printf("__int128 max  %016" PRIx64 "%016" PRIx64 "\n",(uint64)(x>>64),(uint64)x);
 
}

