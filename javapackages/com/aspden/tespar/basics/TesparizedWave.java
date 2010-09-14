/*
 * TesparizedWave.java
 */

package com.aspden.tespar.basics;

import java.io.*;
import java.awt.*;
import com.aspden.graph.*;
import com.aspden.tespar.codebooks.*;
import com.aspden.tespar.utility.*;

/**
 * A TesparizedWave is three streams of numbers representing the duration, shape and magnitude of the tespar symbols in a waveform.
 */
public class TesparizedWave extends Object {
    private int[] duration;
    private int[] shape;
    private double[] magnitude;
    private int originallength;


    /** Creates new TesparizedWave from a SoundWave
     * @param theSoundWave The SoundWave to be processed.
     */
    public TesparizedWave(SoundWave theSoundWave) {
        double[] a=theSoundWave.getData();
        this.originallength=a.length;
        int[] d=new int[a.length];
        int[] s=new int[a.length];
        double[] m=new double[a.length];
        int symbolcount=0;

        int len=1;
        int minima=0;
        double magnitude=abs(a[0]);
        for(int i=1; i<a.length-1; i++) //all internal points
        {
            if (sign(a[i])!=sign(a[i-1])) //zero crossing
            {
                //new symbol on vector
                d[symbolcount]=len;
                s[symbolcount]=minima;
                m[symbolcount]=magnitude;
                symbolcount++;
                //reset duration, minima, magnitude
                len=1; minima=0; magnitude=abs(a[i]);
            }
            else
            {
                //increase duration
                len++;
                //check maximum
                if(abs(a[i])>magnitude) magnitude=abs(a[i]);
                //if extremum record it.
                int grad1=sign(a[i]-a[i-1]);
                int grad2=sign(a[i+1]-a[i]);
                if( (grad1!=sign(a[i]))  && (grad1!=grad2) ) minima++;
            }

        }
        
        if(symbolcount==0){ //Check for the case where there are no symbols.
            System.out.println("NULL WAVE DETECTED: No zero crossings");
            //throw new IllegalArgumentException("Can't do TESPAR on a stream with no zero crossings!"); 
                d[symbolcount]=1;
                s[symbolcount]=0;
                m[symbolcount]=0;
                symbolcount++;
        }

        //trim the arrays
        this.duration=new int[symbolcount];
        this.shape=new int[symbolcount];
        this.magnitude=new double[symbolcount];
        System.arraycopy(d,0,this.duration,0, symbolcount);
        System.arraycopy(s,0,this.shape,0, symbolcount);
        System.arraycopy(m,0,this.magnitude,0, symbolcount);
    }

    /** Displays the TesparizedWave with its natural symbols encoded by a codebook, although still with the time axis rescaled to show true time rather than 'epochs elapsed'.
     * @param c The {@link Codebook} to be used in the translation.
     * @return A display of coded symbols.
     */
    public Component getCodedDisplay(Codebook c)
    {
        int[] d=getDurationRescaledDuration();
        int[] s=getDurationRescaledShape();
        double[] m=getDurationRescaledMagnitude();

        double[] sym=new double[m.length];

        for(int i=0; i<m.length; i++) sym[i]=c.getSymbol(d[i], s[i], m[i]);
        return new ArrayDisplayPanel(sym,0,c.getCodebookSize());
    }

    /*public double[] getSlidingInnerProduct(Codebook c, int window)
    {

        int[] d=getDurationRescaledDuration();
        int[] s=getDurationRescaledShape();
        double[] m=getDurationRescaledMagnitude();

        double[] ip=new double[d.length];

        int leftWindowMin=0;
        int leftWindowMax=window-1;
        int rightWindowMin=window;
        int rightWindowMax=2*window-1;
        int ippos=0;

        while(rightWindowMax<d.length)
        {
            double[] leftWindow=new double[c.getCodebookSize()];
            double[] rightWindow=new double[c.getCodebookSize()];
            for(int i=leftWindowMin; i<=leftWindowMax; i++)
            {
                leftWindow[c.getSymbol(d[i], s[i], m[i])]++;
            }
            for(int i=rightWindowMin; i<=rightWindowMax; i++)
            {
                rightWindow[c.getSymbol(d[i], s[i], m[i])]++;
            }
            ip[ippos]=Mathematics.innerProduct(leftWindow, rightWindow)/Math.sqrt(Mathematics.innerProduct(rightWindow, rightWindow)*Mathematics.innerProduct(leftWindow, leftWindow));


            leftWindowMin++;
            leftWindowMax++;
            rightWindowMin++;
            rightWindowMax++;
            ippos++;
        }
        return ip;
    }*/

    /*public double[] getSlidingInnerProduct(Codebook c, int window)
    {

        int[] d=getDurationRescaledDuration();
        int[] s=getDurationRescaledShape();
        double[] m=getDurationRescaledMagnitude();

        double[] ip=new double[d.length];

        int leftWindowMin=0;
        int leftWindowMax=window-1;
        int rightWindowMin=window;
        int rightWindowMax=2*window-1;
        int ippos=0;

        double[] leftWindow=new double[c.getCodebookSize()];
        double[] rightWindow=new double[c.getCodebookSize()];
        for(int i=leftWindowMin; i<=leftWindowMax; i++)
        {
            leftWindow[c.getSymbol(d[i], s[i], m[i])]++;
        }
        for(int i=rightWindowMin; i<=rightWindowMax; i++)
        {
            rightWindow[c.getSymbol(d[i], s[i], m[i])]++;
        }
        ip[ippos]=Mathematics.innerProduct(leftWindow, rightWindow)/Math.sqrt(Mathematics.innerProduct(rightWindow, rightWindow)*Mathematics.innerProduct(leftWindow, leftWindow));

        while(rightWindowMax<d.length-1)
        {


            leftWindow[c.getSymbol(d[leftWindowMin], s[leftWindowMin], m[leftWindowMin])]--;
            rightWindow[c.getSymbol(d[rightWindowMin], s[rightWindowMin], m[rightWindowMin])]--;
            leftWindowMin++;
            leftWindowMax++;
            rightWindowMin++;
            rightWindowMax++;
            ippos++;
            leftWindow[c.getSymbol(d[leftWindowMax], s[leftWindowMax], m[leftWindowMax])]++;
            rightWindow[c.getSymbol(d[rightWindowMax], s[rightWindowMax], m[rightWindowMax])]++;
            
            ip[ippos]=Mathematics.innerProduct(leftWindow, rightWindow)/Math.sqrt(Mathematics.innerProduct(rightWindow, rightWindow)*Mathematics.innerProduct(leftWindow, leftWindow));
            
            
        }
        return ip;
    }*/
   
    public Component getSlidingInnerProductDisplay(Codebook c, int window)
    {
        return new ArrayDisplayPanel(getSlidingInnerProduct(c,window),0,1);
    }
    
    private double[] getSlidingInnerProduct(Codebook c, int window)
    {

        int[] d=getDurationRescaledDuration();
        int[] s=getDurationRescaledShape();
        double[] m=getDurationRescaledMagnitude();

        double[] ip=new double[d.length];

        int leftWindowMin=0;
        int leftWindowMax=window-1;
        int rightWindowMin=window;
        int rightWindowMax=2*window-1;
        int ippos=window-1;
        
        if(d.length<rightWindowMax) return ip; //If the wave is not long enough to accommodate two window lengths give up and send back zeroes.

        double[] leftWindow=new double[c.getCodebookSize()];
        double[] rightWindow=new double[c.getCodebookSize()];
        for(int i=leftWindowMin; i<=leftWindowMax; i++)
        {
            leftWindow[c.getSymbol(d[i], s[i], m[i])]++;
        }
        for(int i=rightWindowMin; i<=rightWindowMax; i++)
        {
            rightWindow[c.getSymbol(d[i], s[i], m[i])]++;
        }
        double crossip=Mathematics.innerProduct(leftWindow, rightWindow);
        double leftip=Mathematics.innerProduct(leftWindow, leftWindow);
        double rightip=Mathematics.innerProduct(rightWindow, rightWindow);
        ip[ippos]=crossip/Math.sqrt(leftip*rightip);

        while(rightWindowMax<d.length-1)
        {

            int leftlose=c.getSymbol(d[leftWindowMin], s[leftWindowMin], m[leftWindowMin]);
            crossip-=rightWindow[leftlose];
            leftip-=2*leftWindow[leftlose]-1;
            leftWindow[leftlose]--;
            
            int rightlose=c.getSymbol(d[rightWindowMin], s[rightWindowMin], m[rightWindowMin]);
            crossip-=leftWindow[rightlose];
            rightip-=2*rightWindow[rightlose]-1;
            rightWindow[rightlose]--;
            
            leftWindowMin++;
            leftWindowMax++;
            rightWindowMin++;
            rightWindowMax++;
            ippos++;
            
            int leftgain=c.getSymbol(d[leftWindowMax], s[leftWindowMax], m[leftWindowMax]);
            crossip+=rightWindow[leftgain];
            leftip+=2*leftWindow[leftgain]+1;
            leftWindow[leftgain]++;
            
            int rightgain=c.getSymbol(d[rightWindowMax], s[rightWindowMax], m[rightWindowMax]);
            crossip+=leftWindow[rightgain];
            rightip+=2*rightWindow[rightgain]+1;
            rightWindow[rightgain]++;
            
            ip[ippos]=crossip/Math.sqrt(rightip*leftip);
            
            
        }
        return ip;
    }

    private int getDurationRescaledLength()
    {
        int pos=0;
        for(int i=0; i<duration.length; i++)
        {
            pos+=duration[i];
        }
        return pos;
    }

    private int[] getDurationRescaledDuration()
    {
        int[] d=new int[getDurationRescaledLength()];
        int pos=0;
        for(int i=0; i<duration.length; i++)
        {
            for(int j=0; j<duration[i]; j++)
            {
                d[pos]=duration[i];
                pos++;
            }
        }
        return d;
    }

    private int[] getDurationRescaledShape()
    {
        int [] s=new int[getDurationRescaledLength()];
        int pos=0;
        for(int i=0; i<duration.length; i++)
        {
            for(int j=0; j<duration[i]; j++)
            {
                s[pos]=shape[i];
                pos++;
            }
        }
        return s;
    }

    private double[] getDurationRescaledMagnitude()
    {
        double[] m=new double[getDurationRescaledLength()];
        int pos=0;
        for(int i=0; i<duration.length; i++)
        {
            for(int j=0; j<duration[i]; j++)
            {
                m[pos]=magnitude[i];
                pos++;
            }
        }
        return m;
    }

    /** Gets a display of the tesparized Wave.
     * repeats each natural symbol according to its duration so that the axis is scaled like the original wave.
     * @return A graphical component representing the TESPAR stream.
     */
    public Component getDisplay()
    {
        return new ArrayDisplayPanel(getDurationRescaledMagnitude(),0,10000,int2double(getDurationRescaledShape()),-10,10);
    }

    private static final int sign(double x){
        if(x>=0) return 1;
        else return -1;
    }

    private static final double abs(double x){
        if(sign(x)==1) return x;
        else return -x;
    }

    private static double[] int2double(int[] a)
    {
        double[] d=new double[a.length];
        for(int i=0; i<a.length; i++) d[i]=(double)a[i];
        return d;
    }

    /** Returns an array of durations of TESPAR symbols in time order.
     * @return The durations of the TESPAR symbols.
     */
    public int[] getDurations()
    {
        return (int[])duration.clone();
    }
    /** Returns an array of shapes of TESPAR symbols in time order.
     * @return The shapes of the TESPAR symbols.
     */
    public int[] getShapes()
    {
        return (int[])shape.clone();
    }
    /** Returns an array of magnitudes of TESPAR symbols in time order.
     * @return The magnitudes of the TESPAR symbols.
     */
    public double[] getMagnitudes()
    {
        return (double[])magnitude.clone();
    }


    /** Gets the original length of the wave before it was TESPARized.
     * @return the original length of the wave (in samples).
     */
    public int getOriginalLength()
    {
        return originallength;
    }

    /**
     * Test/Demo code
     * @param args the command line arguments
     */
    public static void main(String args[]) throws IOException
    {
        String filename="C:\\speech databases\\jamesdigitsuniversity\\record00\\6.txt"; int freq=16000;
        SoundWave s=new SoundWave(new java.io.File(filename), freq);
        s.filterButterworthHighPassOrder8();
        TesparizedWave w = new TesparizedWave(s);
        new SingleComponentFrame(new ArrayDisplayPanel(int2double(w.duration),int2double(w.shape)),"Duration & Shape");
        new SingleComponentFrame(new ArrayDisplayPanel(int2double(w.duration),w.magnitude),"Duration & Magnitude");
        new SingleComponentFrame(w.getDisplay(),"Natural Display");
        new SingleComponentFrame(w.getCodedDisplay(new Tespar20kHzCodebook(0)), "Tespar20kHzCodebook(0)");
        new SingleComponentFrame(w.getCodedDisplay(new Tespar20kHzCodebook(50)), "Tespar20kHzCodebook(50)");
        new SingleComponentFrame(w.getCodedDisplay(new Tespar20kHzCodebook_Tolerant(50)), "Tespar20kHzCodebook_Tolerant(50)");
        new SingleComponentFrame(w.getCodedDisplay(new JohnCodebook()), "JohnCodebook");
        double[] ip=w.getSlidingInnerProduct(new Tespar20kHzCodebook_Tolerant(50),1000);
        new SingleComponentFrame(new ArrayDisplayPanel(ip,0,1), "w.getSlidingInnerProduct()");
    }

}