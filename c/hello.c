#include<stdio.h>

//#define ADEFINE 23
#define ADEFINE "23"
//#undef ADEFINE

#define M1(f) #f

#define M(f)  M1(f)

#ifdef ADEFINE
#pragma message( "The value of ADEFINE is: " M(ADEFINE) )
#else
#pragma message( "ADEFINE is undefined" )
#endif




typedef struct object_s
{
  int *a;
  size_t sizeof_a;
  int *b;
  size_t sizeof_b;
  int c;
  const char *name;
} object_t ;

#define DECLARE_OBJECT(a,b,c,d)   { (a), (sizeof(a)/(sizeof(int))),(b), sizeof(b)/(sizeof(int)), (c),(d)}

object_t thing[] = {
  DECLARE_OBJECT(((int[]){4,3,2}), ((int[]){ 2, 0, 1 }), 1234, "arfle"),
  DECLARE_OBJECT(((int[]){1,2})  , ((int[]){ 2, 0 })   ,  234, "foo")
};

int main(void) {

#ifdef DOOM
  printf("doom");
#else
  printf("no doom");
#endif

  int i,j;
  for(j=0; j<((sizeof(thing)/(sizeof(object_t)))); j++){
    printf("%s\n", thing[j].name);
    for(i=0; i<thing[j].sizeof_a; i++) printf("%d, ", thing[0].a[i] );
    printf("\n");
    for(i=0; i<thing[j].sizeof_b; i++) printf("%d, ", thing[0].b[i] );
    printf("\n");
    printf("%d\n",thing[j].c);
  }
    return 0;
}
