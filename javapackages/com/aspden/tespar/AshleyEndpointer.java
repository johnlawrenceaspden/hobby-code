/*
 * AshleyEndpointer.java
 */
package com.aspden.tespar;

import com.aspden.tespar.basics.*;
import com.aspden.graph.*;

import java.io.*;

public class AshleyEndpointer extends Object {
    private int window;
    private double lowthreshold;
    private double highthreshold;

    /** Creates new AshleyEndpointer */
    public AshleyEndpointer(int window, double lowthreshold, double highthreshold) {
        this.window=window;
        this.lowthreshold=lowthreshold;
        this.highthreshold=highthreshold;
    }
    
    private void EnergyDisplay(SoundWave s, String title)
    {
        double[] energy=calculateEnergy(s.data, this.window);
        new SingleComponentFrame(new ArrayDisplayPanel(s.data,-10000,10000,energy,-10000,10000),title);
    }
    
    private void EnergyDisplay(double[] data, double[] energy, String title)
    {
        new SingleComponentFrame(new ArrayDisplayPanel(data,-10000,10000,energy,-10000,10000),title);
    }
    
    private void Display(double[] s, String title)
    {
        new SingleComponentFrame(new ArrayDisplayPanel(s,-10000,10000),title);
    }
    
    public SoundWave endpoint(SoundWave s)
    {
        EnergyDisplay(s, "as read");
        SoundWave highpass=s.filterButterworthHighPassOrder8();
        EnergyDisplay(highpass, "high pass");
        SoundWave formant=s.filterButterworthFormantPassOrder2();
        EnergyDisplay(formant, "formant pass");
        SoundWave hiss=s.filterButterworthHissPassOrder2();
        EnergyDisplay(hiss, "hiss pass");
        
        //double[] wave=addArrays(formant.data, hiss.data);
        double[] wave=highpass.data;
        
        double[] waveenergy=calculateEnergy(wave, this.window);
        EnergyDisplay(wave, waveenergy, "wave energy");
        double[] diffwaveenergy=absdifferentiate(waveenergy);
        EnergyDisplay(wave, diffwaveenergy, "diff(wave energy)");
        double[] combination=addArrays(waveenergy, diffwaveenergy);
        Display(combination, "combination");
        
        double[] sorted=sort(combination);
        Display(sorted, "combination");
     
        return s;
    }
    
    private double[] sort(double[] a)
    {
        double[] sorted=(double[])(a.clone());
        java.util.Arrays.sort(sorted);
        return sorted;
    }
    
    private double[] calculateEnergy(double[] data, int windowsize)
    {
        double[] energy=new double[data.length];

        double ensquared=0;
        int halfwindow=windowsize/2;

        if(windowsize>data.length) //The wave doesn't have enough samples for this calculation to be meaningful
        {
            System.out.println("NOT ENOUGH DATA FOR ENERGY CALCULATION!!");
            return energy;
        }

        for(int i=0; i<windowsize; i++) ensquared+=data[i]*data[i];
        for(int i=0; i<halfwindow; i++) energy[i]=(ensquared/windowsize);
        for(int i=halfwindow; i<data.length-halfwindow; i++)
        {
            ensquared+=data[i+halfwindow]*data[i+halfwindow];
            ensquared-=data[i-halfwindow]*data[i-halfwindow];
            energy[i]=(ensquared/windowsize);
        }
        for(int i=data.length-halfwindow; i<data.length; i++) energy[i]=(ensquared/windowsize);
        
        for(int i=0; i<data.length; i++) energy[i]=Math.sqrt(energy[i]);

        return energy;
    }
    
    private double[] addArrays(double[] a, double[] b)
    {
        if(a.length!=b.length) throw new IllegalArgumentException("Arrays to be added are not the same length!");
        double[] c=new double[a.length];
        for(int i=0; i<a.length; i++)
        {
            c[i]=a[i]+b[i];
        }
        return c;
    }
    
    private double[] absdifferentiate(double[] a)
    {
        double[] diff=new double[a.length];
        diff[0]=0;
        for(int i=1; i<a.length; i++)
        {
            diff[i]=(this.window)*Math.abs(a[i]-a[i-1]);
        }
        return diff;
    }

    
    
    /**
     * @param args the command line arguments
     */
    public static void main (String args[]) throws IOException {
        SoundWave s = new SoundWave(new java.io.File("C:\\ddldata\\mrdh_n2s3d1.wav"));
        AshleyEndpointer ae= new AshleyEndpointer(1000, 3,10);
        SoundWave n = ae.endpoint(s);
    }

}