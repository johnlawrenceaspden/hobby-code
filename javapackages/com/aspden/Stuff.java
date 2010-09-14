package com.aspden;

public class Stuff extends Object {

    public static void threadSleep(int i)
    {
        try{
            Thread.sleep(i);
        }catch(InterruptedException e){
        }
    }
    /** Turn an array into a string of values.
     * @param a the array
     * @return the string
     */
    public static String stringify(int[] a) {
        String s="";
        for(int i=0; i<a.length; i++) s+=a[i]+" ";
        return s;
    }

    /** Turn an array into a string of values.
     * @param a the array
     * @return the string
     */
    public static String stringify(double[] a)
    {
        String s="";
        for(int i=0; i<a.length; i++) s+=a[i]+" ";
        return s;
    }

    public static double[] int2Double(int[] a)
    {
        double[] d=new double[a.length];
        for(int i=0; i<a.length; i++) d[i]=a[i];
        return d;
    }
    
    public static double[][] int2Double(int[][] a)
    {
        double[][] d=new double[a.length][];
        for(int i=0; i<a.length; i++) d[i]=int2Double(a[i]);
        return d;
    }
    
    public static String[] concat(String[] a, String[]b)
    {
        String[] c=new String[a.length+b.length];
        int cp=0;
        for(int i=0; i<a.length; i++) {
            c[cp++]=a[i];
        }
        for(int i=0; i<b.length; i++) {
            c[cp++]=b[i];
        }
        return c;
    }
    
    public static String[] concat(String[] a, String[]b, String[] c)
    {
        return concat(concat(a,b),c);
    }

}