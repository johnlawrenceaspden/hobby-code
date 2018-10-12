/*Skeleton C program, load into emacs should provoke C mode, F9 runs makefile and output should appear in compilation window */
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<stdarg.h>
#include<bsd/string.h>
#include"debug.h"
#include"module.h"

#define DEBUG 0







int main(void){
  char s[100]="";
  DBGS(s);
  DBGI(sizeof(s));
  DBGI(snprintf(DBGS(s),DBGI(sizeof(s)),"yo%i",2));
  DBGS(s);
  printf("hello\n");
  yo();
  for(int i=1; i<10; i++) {
    DBGI(i);
    DBGI(snprintf(s,sizeof(s),"%i",i));
    DBGI(strlcat(s,"hello",sizeof(s)));
    printf("%s",s);
  }
}

