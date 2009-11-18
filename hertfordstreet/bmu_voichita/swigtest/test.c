#include "test.h"

int doom(void){
    return 5;
}

double ddoom(double a){
    return a+1;
}

int product(int n, int *vars)
{
    int i;
    int product=1;
    for(i=0; i<n; i++) product*=vars[i];
    return product;
}

double dproduct(int n, double *vars)
{
    int i;
    double product=1;
    for(i=0; i<n; i++) product*=vars[i];
    return product;
}


