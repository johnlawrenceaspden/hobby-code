#include<stdio.h>

main()
{
  float fahr, celsius;
  int lower, upper, step;

  lower = -20;
  upper = 150;
  step = 10;

  printf("%3s %6s\n", "F", "C");
  while (celsius <= upper) {
    fahr = (9.0/5.0)*celsius+32.0;
    printf("%3.0f %6.1f\n", fahr, celsius);
    celsius = celsius + step;
  }
} 
