/*
 * SMatrixGram.java
 */

package com.aspden.tespar.statistics;

import com.aspden.tespar.*;
import com.aspden.tespar.codebooks.*;
import com.aspden.tespar.basics.*;
import com.aspden.tespar.utility.*;


import java.util.*;
import java.text.*;
import java.awt.*;

public class SMatrixGram extends Statistic {
    private Codebook theCodebook;
    private int slices;
    private double[][] occurrences;

    /*Set up the blank matrix given the codebook and name. Common code from both constructors*/
    private void CreateMatrix(Codebook c, int slices){
        theCodebook=c;
        this.slices=slices;
        occurrences=new double[slices][];
        for(int i=0; i<slices; i++) occurrences[i]=new double[theCodebook.getCodebookSize()];
    }

    public SMatrixGram(TesparizedWave w, Codebook c, int slices, boolean durationsplit)
    {
        CreateMatrix(c, slices);
        int[] duration=w.getDurations();
        int[] shape=w.getShapes();
        double[] magnitude=w.getMagnitudes();
        int len=duration.length;
        if (len==0) throw new IllegalArgumentException("Tried to construct S-Matrix from empty symbol stream");
        
        if(durationsplit)
        {
            int originallength=w.getOriginalLength();
            int time=0;
            for(int i=0; i<len; i++)
            {
                occurrences[time*slices/originallength][c.getSymbol(duration[i], shape[i], magnitude[i])]+=duration[i] ;
                time+=duration[i];
            }
        }
        else
        {
            for(int i=0; i<len; i++)
            {
                occurrences[i*slices/len][c.getSymbol(duration[i], shape[i], magnitude[i])]+=duration[i] ;
            }
        }

    }

    /**Creates an average SMatrixGram from those on the list.*/
    SMatrixGram(java.util.List l)
    {
        if(l.isEmpty()) throw new IllegalArgumentException("Can't take average of empty list");
        SMatrixGram sm=(SMatrixGram)l.get(0);
        CreateMatrix(sm.getCodebook(),sm.slices);

        for(Iterator it=l.iterator(); it.hasNext();)
        {
            sm=(SMatrixGram)it.next();
            if(sm.getCodebook()!=this.theCodebook) throw new IllegalArgumentException("different codebooks on list.");
            if(this.slices!=sm.slices) throw new IllegalArgumentException("different nos of slices on list.");

            /**NOTE HAVE REMOVED THE NORMALIZATION CODE**/

            //double norm=Math.sqrt(innerProduct(sm,sm));

            for(int i=0;i<this.theCodebook.getCodebookSize();i++)
            {
                for(int j=0; j<this.slices; j++)
                {
                    this.occurrences[j][i]+=sm.occurrences[j][i];                    // /norm
                }
            }
        }
    }

    public Statistic asAverage(java.util.List list)
    {
        return new SMatrixGram(list);
    }


    public double correlate(Statistic a)
    {
        return angle(this, (SMatrixGram)a);
    }

    public static double angle(SMatrixGram a, SMatrixGram b)
    {
        double x=innerProduct(a,b);
        x/=Math.sqrt(innerProduct(a,a));
        x/=Math.sqrt(innerProduct(b,b));

        //Catch numerical sillinesses
        if(x>1.000001) throw new IllegalArgumentException("angle calculation gone haywire x="+x);
        if(x>1.0) x=1.0;
        if(x<-1.000001) throw new IllegalArgumentException("angle calculation gone haywire x="+x);
        if(x<-1.0) x=-1.0;

        x=Math.acos(x);
        x/=Math.PI;
        x*=180;
        return x;
    }

    private static double innerProduct(SMatrixGram a, SMatrixGram b)
    {
        double ip=0;
        if(a.slices!=b.slices) throw new IllegalArgumentException("different nos of slices");
        for(int i=0; i<a.slices; i++)
        {
            ip+=Mathematics.innerProduct(a.occurrences[i], b.occurrences[i]);
        }
        return ip;
    }


    public String toString()
    {
        String s="";
        NumberFormat f=NumberFormat.getInstance();
        f.setMinimumIntegerDigits(3);
        s+=getStatisticType()+"\n";
        for(int j=0; j<this.slices; j++)
        {
            for(int i=0;i<this.theCodebook.getCodebookSize();i++)
            {
                s+=f.format(occurrences[j][i])+"|";
            }
            s+="\n";
        }
        return s;
    }

    private double[] concatenate(double[] a, double[] b)
    {
        double[] c=new double[a.length+b.length];
        for(int i=0; i<a.length; i++) c[i]=a[i];
        for(int i=0; i<b.length; i++) c[i+a.length]=b[i];
        return c;
    }

    public Component visualise()
    {
        double[] d=new double[0];
        for(int i=0; i<slices; i++) d=concatenate(d, occurrences[i]);
        return new com.aspden.graph.ArrayDisplayPanel(d);
    }

    public Component getInternalAnglesDisplay()
    {
        double[] d=new double[slices-1];
        for(int i=0; i<(slices-1);i++)
        d[i]=Mathematics.innerProduct(occurrences[i], occurrences[i+1])/Math.sqrt(Mathematics.innerProduct(occurrences[i], occurrences[i])*Mathematics.innerProduct(occurrences[i+1], occurrences[i+1]));

        return new com.aspden.graph.ArrayDisplayPanel(d,0,1);
    }

    public Codebook getCodebook()
    {
        return theCodebook;
    }

    public String getStatisticType()
    {
        return "SMatrixGram (Duration Weighted) "+" slices "+this.slices;
    }
}


