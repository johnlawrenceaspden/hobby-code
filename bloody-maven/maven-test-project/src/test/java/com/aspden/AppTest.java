package com.aspden;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import com.aspden.app;

/**
 * Unit test for simple App.
 */
public class AppTest 
    extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public AppTest( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( AppTest.class );
    }

    /**
     * 
     */
    public void testApp()
    {
        assertTrue( App.factorial(5) == 1*2*3*4*5 );
        assertTrue( App.factorial(0) == 1 );
        assertTrue( (new app()).factorial(4) == 1*2*3*4 );
    }
}
