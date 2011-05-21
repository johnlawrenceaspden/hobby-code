#include<stdio.h>
#include<string.h>
#include<stdlib.h>

char* stringreverse1(char* str)
{
  char*a = str;
  char*b = str;
  while(*b!=0){
    b++;
  }
  b--;
  while(b>a){
    char c;
    c=*a;
    *a=*b;
    *b=c;
    b--;
    a++;
  }
  return str;
}

char* stringreverse2(char* str)
{
  char *copy, *cp;
  int len = strlen(str);
  copy = (char*) malloc(len+1);
  cp = copy+len;
  *cp--='\0';
  while(cp>=copy){
    printf("%c,%c\n", *cp, *str);
    *cp--=*str++;
    printf("%c,%c\n", *cp, *str);
  }

  printf("%s\n", copy);
  return copy;
}

int main(void)
{
  char str[] = "heloworld";
  
  printf("%s\n", stringreverse1(str));
  char* copy;
  
  copy = stringreverse2(str);
  printf("%s\n", copy);
  free(copy);

  return 0;
}
