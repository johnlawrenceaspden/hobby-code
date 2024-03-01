#include<stdio.h>
#include<malloc.h>
#include<assert.h>
#include<stdlib.h>
#include<stdbool.h>
#include<string.h>

char* maximumOddBinaryNumber(char* s);

char* maximumOddBinaryNumber(char* s){
  int ones=0;
  int zeros=0;
  char* p=s;
  char c;
  while(c=*p++)
    {if('0'==c) zeros++; else ones++;}
  char* res = malloc(ones+zeros+1);
  p=res;
  ones--;
  while(ones--) *p++='1';
  while(zeros--) *p++='0';
  *p++='1';
  *p=0;
  return res;
}








void Test(char* s, char* r)
{
  char*res = maximumOddBinaryNumber(s);
  printf("%s->%s(expected %s)\n",s,res,r);
  assert(strcmp(res,r)==0);
}


int main(void)
{
  printf("yo\n");

  Test("010","001");
  Test("0101","1001");

  return 0;
}
