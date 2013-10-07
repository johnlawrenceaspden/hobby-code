#include<stdio.h>

unsigned mystery(unsigned a) {
  unsigned c = (a & -a);
  unsigned r = a+c;
  return ((((r ^ a) >> 2) / c) | r);
}

#define BYTETOBINARYPATTERN "%d%d%d%d%d%d%d%d"
#define BYTETOBINARY(byte)  \
  (byte & 0x80 ? 1 : 0), \
  (byte & 0x40 ? 1 : 0), \
  (byte & 0x20 ? 1 : 0), \
  (byte & 0x10 ? 1 : 0), \
  (byte & 0x08 ? 1 : 0), \
  (byte & 0x04 ? 1 : 0), \
  (byte & 0x02 ? 1 : 0), \
  (byte & 0x01 ? 1 : 0) 

void main(void)
{
  
  unsigned doom=7;
  for (int i=0; i< 100; i++) {
    printf ("%3d -> "BYTETOBINARYPATTERN"\n", doom,  BYTETOBINARY(doom));
    doom=mystery(doom);
  }
}

/* john@dell-mini-2$ gcc -std=gnu99 mystery.c */
/* ~/hobby-code */
/* john@dell-mini-2$ ./a.out */
/*   7 -> 00000111 */
/*  11 -> 00001011 */
/*  13 -> 00001101 */
/*  14 -> 00001110 */
/*  19 -> 00010011 */
/*  21 -> 00010101 */
/*  22 -> 00010110 */
/*  25 -> 00011001 */
/*  26 -> 00011010 */
/*  28 -> 00011100 */
 
