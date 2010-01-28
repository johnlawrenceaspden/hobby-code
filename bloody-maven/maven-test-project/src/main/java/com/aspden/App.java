package com.aspden;

public class App 
{
    public static void main( String[] args )
    {
        System.out.println( "Hello World!" );
        System.out.println( "the factorial of 10 is " + factorial(10));
    }

    public static int factorial (int n)
    {
        int fac=1;
        for (int i=2; i<=n; i++){
            fac=fac*i;
        }
        return fac;
    }
}
