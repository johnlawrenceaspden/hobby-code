#include <stdio.h>

main()
{
  int c; 
  int d;
  while (d= ((c=getchar()) != EOF))
    printf("%d (%d), ",c,d);
  printf("%d (%d)\n",c,d);
  printf("The value of EOF is %d\n", EOF);
}
