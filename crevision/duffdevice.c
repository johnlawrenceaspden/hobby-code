#include<stdio.h>
#include<stdlib.h>
#include<string.h>

void copy1(char *copy, char *str, int len)
{
  memcpy(copy, str, len);
}

void copy2(char *copy, char *str, int len)
{
  while(len>0){
    *copy++=*str++;
    len--;
  }
}

void copy3(char *copy, char *str, int len)
{
  int halflen=len/2;
  int extra=len-2*halflen;
  if(extra){
    *copy++=*str++;
  }
  while(halflen>0){
    *copy++=*str++;
    *copy++=*str++;
    halflen--;
  }
}

void copy4(char *copy, char *str, int len)
{
  int halflen=len/2;
  int extra=len-2*halflen;
  switch(extra){
    while(halflen>=0){
    case 0:  *copy++=*str++;
    case 1:  *copy++=*str++;
      halflen--;
    }
  }
}

void copy5(char *copy, char *str, int len)
{
  int div=len/8;
  int mod=len-8*div;
  switch(mod){
    while(div>=0){
    case 7:  *copy++=*str++;
    case 6:  *copy++=*str++;
    case 5:  *copy++=*str++;
    case 4:  *copy++=*str++;
    case 3:  *copy++=*str++;
    case 2:  *copy++=*str++;
    case 1:  *copy++=*str++;
    case 0:  *copy++=*str++;
      div--;
    }
  }
}


void duffsdevice(char *copy, char *str, int len)
{
  len=len/8;
  switch(len%8){
    while(len>=0){
    case 7:  *copy++=*str++;
    case 6:  *copy++=*str++;
    case 5:  *copy++=*str++;
    case 4:  *copy++=*str++;
    case 3:  *copy++=*str++;
    case 2:  *copy++=*str++;
    case 1:  *copy++=*str++;
    case 0:  *copy++=*str++;
      len--;
    }
  }
}

int main (void)
{
  char str[] = "heloworldheloworld";
  void(*f[])(char*, char*, int)={copy1, copy2, copy3, copy4, copy5, duffsdevice};
  int i;
  for(i=0; i<5; i++){
    char* copy = malloc(strlen(str)+1);
    (*f[i])(copy, str, strlen(str)+1);
    printf("%s\n", str);
    printf("%s\n", copy);
    free(copy);
  }
  return 0;
}
