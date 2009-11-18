#include <stdio.h>
#include "bmu.h"

int main(void)
{
    const double signal[]={1,2,3,4,5,6,7,8};
    const int length=8;
    const int waveletsteps=2;
    const int hsteps=100;
    const double initialguess=0.5;

    double H, sigma;

    VoichitaNumber(signal,length,waveletsteps,hsteps,initialguess,&H, &sigma);

    printf("estimate of H     %f\n", H);
    printf("estimate of sigma %f\n", sigma);
    return 0;
}
