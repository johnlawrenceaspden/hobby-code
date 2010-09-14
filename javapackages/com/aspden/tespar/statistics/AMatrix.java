/*
 * AMatrix.java
 */

package com.aspden.tespar.statistics;

import com.aspden.tespar.*;
import com.aspden.tespar.basics.*;
import com.aspden.tespar.codebooks.*;
import com.aspden.graph.*;

import java.util.*;
import java.text.*;
import java.awt.*;

/**An A Matrix is a standard TESPAR statistic which represents the occurrence of pairs of symbols.
they come in many varieties, such as frequency/duration weighted/magnitude weighted*/
public class AMatrix extends Statistic {
  private Codebook theCodebook;
  private double[][] successions;
  
 /*Set up the blank matrix given the codebook and name. Common code from both constructors*/
  private void CreateMatrix(Codebook c){
    theCodebook=c;
    successions=new double[theCodebook.getCodebookSize()][];
    for(int i=0; i<theCodebook.getCodebookSize();i++)
    {
      successions[i]=new double[theCodebook.getCodebookSize()];
    }
  }

 /**Fill the matrix with data from a symbol stream. Use the stream's name as the name of the A matrix*/
  public AMatrix(TesparizedWave w, Codebook c) {
    //Initialise the empty matrix.
    CreateMatrix(c);
        int[] duration=w.getDurations();
        int[] shape=w.getShapes();
        double[] magnitude=w.getMagnitudes();
        
        /*go through the stream. For each pair of symbols add the combined duration to the matrix at the point representing that pair*/
        int len=duration.length;
        if (len==0) throw new IllegalArgumentException("Tried to construct A-Matrix from empty symbol stream");
        for(int i=0; i<len-1; i++)
        {
            successions[c.getSymbol(duration[i], shape[i], magnitude[i])][c.getSymbol(duration[i+1], shape[i+1], magnitude[i+1])]+=duration[i]+duration[i+1];
        }

  }

  /**Creates an average AMatrix from those on the list.*/
  AMatrix(java.util.List l)
  {
    if(l.isEmpty()) throw new IllegalArgumentException("Can't take average of empty list");
    AMatrix am=(AMatrix)l.get(0);
    CreateMatrix(am.getCodebook());

    for(Iterator it=l.iterator(); it.hasNext();)
    {
      am=(AMatrix)it.next();
      if(am.getCodebook()!=this.theCodebook) throw new IllegalArgumentException("AMatrices on list have different codebooks.");
      
      double norm=Math.sqrt(innerProduct(am,am)); //seems to make little difference
      
      for(int i=0;i<this.theCodebook.getCodebookSize();i++)
      {
        for(int j=0; j<this.theCodebook.getCodebookSize();j++)
        {
          this.successions[i][j]+=am.successions[i][j]/norm;
        }
      }
    }
  }
  
  public Statistic asAverage(java.util.List list)
  {
    return new AMatrix(list);
  }

  
  public double correlate(Statistic a)
  {
    return angle(this, (AMatrix)a);
  }

  public static double angle(AMatrix a, AMatrix b)
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

  private static double innerProduct(AMatrix a, AMatrix b)
  {
    return innerProduct(a.successions, b.successions);
  }

  private static double innerProduct(double[][] a, double[][] b)
  {
    if(a.length!=b.length) throw new IllegalArgumentException("Tried to take inner product of differently sized arrays");

    double ip=0;
    for(int i=0; i<a.length; i++)
    {
      if(a[i].length!=b[i].length) throw new IllegalArgumentException("Tried to take inner product of differently sized arrays");
      for(int j=0; j<a[i].length; j++)
      ip+=a[i][j]*b[i][j];
    }

    return ip;
  }

  public String toString()
  {
    String s="";
    NumberFormat f=NumberFormat.getInstance();
    f.setMinimumIntegerDigits(3);
    s+=getStatisticType()+"\n";
    for(int i=0;i<this.theCodebook.getCodebookSize();i++)
    {
      for(int j=0;j<this.theCodebook.getCodebookSize();j++) s+=f.format(successions[i][j])+"|";
      s+="\n";
    }
    return s;
  }
  
  public Component visualise()
  {
    return new ColouredPatchCanvas(successions, new Colourizer.GreyShade());
  }
  
  public Codebook getCodebook()
  {
    return theCodebook;
  }
  
  public String getStatisticType()
  {
    return "AMatrix (Combined Duration Weighted) ";
  }
}