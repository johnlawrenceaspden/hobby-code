#include <stdio.h>
#include <stdlib.h>

double ansa(void)
{
    double x,y,z,sum=0;
    for(int i=0; i<10000000; i++)
    {
        x=drand48();
        y=drand48();
        z=drand48();
        double r= x*x+y*y+z*z;
        //printf("%f %f %f %f\n",x,y,z,r);

        if(r<1) sum++;
    }
    return sum;
}


int main()
{
    //srand48(0xdeadbeef);
    printf("Stochastic Sphere Volume!\n");
    printf("answer=%f", 8*ansa());
    printf("answer=%f", 4*3.14159265358/3);
    return 0;
}
