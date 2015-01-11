#include<stdio.h>
#include<string.h>
#include<stdlib.h>
/* Anyone who is considering a C job will be asked to write a linked
   list implementation at some point*/

typedef struct link
{
  char character;
  struct link *next;
} doom;
  

doom *global=NULL;

char hellostr[]="Hello World";

void plist(doom* lst){
  printf("<");
  for (doom *i=lst;i;i=i->next){
    if(i->character=='\n') printf("\\n"); 
    else printf("%c", i->character);
  }
  printf(">\n");
}

int main (void)
{
  int  length=strlen(hellostr);
  for (int i=0; i<length; i++) {
    char c=hellostr[i];
    doom* newlink=(doom*) malloc(sizeof(doom));
    newlink->character=c;
    newlink->next=global;
    plist(global=newlink);
  }
  
  doom *cursor=global;
  while(cursor){
    printf("%c-", cursor->character);
    cursor=cursor->next;
  }
  printf("\n");

  while(global){
    doom* freeme=global;
    plist(global=global->next);
    free(freeme);
  }
  
  return 0;
}
