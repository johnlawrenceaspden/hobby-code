/*This is a java factorial-calculating class
 In its main method, we'll also use the clojure factorial class*/
package com.aspden;

import com.aspden.app;

public class App 
{
    public static void main( String[] args )
    {
        System.out.println( "Hello World!" );
        System.out.println( "the factorial of 10 is " + factorial(10) + "according to java.");
        System.out.println( "and according to clojure it's " + (new com.aspden.app()).factorial(10) + ".");
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
