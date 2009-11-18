#include<stdio.h>

int f(int i)
{
    printf("%i:",i);
    f(i+1);
}


int main(void)
{
    f(0);
}
