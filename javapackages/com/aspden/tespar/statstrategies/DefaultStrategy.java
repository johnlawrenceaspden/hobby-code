/*
 * DefaultStrategy.java
 */

package com.aspden.tespar.statstrategies;

import com.aspden.tespar.basics.*;
import com.aspden.tespar.codebooks.*;
import com.aspden.tespar.statistics.*;
import com.aspden.graph.*;


import java.io.*;
import java.util.List;
import java.util.*;
import java.awt.*;

public class DefaultStrategy extends StatisticAcquisitionStrategy {

    /*Parameters for the various transformations*/
    private static final boolean LOWPASSFILTER=false;
    private static final boolean HIGHPASSFILTER=true;


    /*AMatrices or SMatrices*/
    private static final int AMATRICES=1, SMATRIXGRAM=2;
    private static final int WHICHSTATISTIC=SMATRIXGRAM;
    private static final int slices=8;

    /*Endpointing: relative or absolute + parameters*/
    private static final boolean RELATIVEENERGYENDPOINTING=true;
    private static final boolean ABSOLUTEENERGYENDPOINTING=false;
    private static final int EnergyEndpointingWindow=1000;
    private static final int RelativeEnergyEndpointingThreshold=(int) Math.sqrt(50000);
    private static final int AbsoluteEnergyEndpointingThreshold=(int) Math.sqrt(50000);

    /*Tespar Processing to be done*/
    private static final boolean DISCARDSMALLSYMBOLS=false;
    private static final int DiscardMagnitude=0;

    /*Codebook to use*/
    //private static final Codebook theCodebook=new JohnCodebook();
    private static final Codebook theCodebook=new Tespar20kHzCodebook_Tolerant(50);
    //private static final Codebook theCodebook=new Tespar20kHzCodebook(50);

    /*Display various graphs as the process runs*/
    private static final boolean DISPLAYFAVOURITES=true;
    private static final boolean DISPLAYALL=false;
    
    private static final boolean DISPLAYWAVES           =false  | DISPLAYALL | DISPLAYFAVOURITES;
    private static final boolean DISPLAYLOWPASSEDWAVES  =false  | DISPLAYALL;
    private static final boolean DISPLAYHIGHPASSEDWAVES =false  | DISPLAYALL;
    private static final boolean DISPLAYPREENDPOINTINGENERGY =false | DISPLAYALL | DISPLAYFAVOURITES;
    private static final boolean DISPLAYPOSTENDPOINTINGENERGY =false | DISPLAYALL;
    private static final boolean DISPLAYENDPOINTEDWAVES =false  | DISPLAYALL| DISPLAYFAVOURITES;
    private static final boolean DISPLAYTESPARIZEDWAVES =false  | DISPLAYALL | DISPLAYFAVOURITES;
    private static final boolean DISPLAYENCODEDWAVES    =false | DISPLAYALL | DISPLAYFAVOURITES;
    private static final boolean DISPLAYSTATISTICS      =false  | DISPLAYALL | DISPLAYFAVOURITES;
    private static final boolean DISPLAYINTERNALANGLES  =false | DISPLAYALL;
    private static final boolean DISPLAYSLIDINGINNERPRODUCTS  =false | DISPLAYALL;

    public String getDescription()
    {
        String s="";
        if(LOWPASSFILTER) s+="Low pass filter;";
        if(HIGHPASSFILTER) s+="High pass filter;";
   
        
        if(RELATIVEENERGYENDPOINTING){
            s+="Endpointing (window size "+EnergyEndpointingWindow+" )";
            s+=" relative with threshold "+RelativeEnergyEndpointingThreshold;
        }
        else if(ABSOLUTEENERGYENDPOINTING)
        {
            s+="Endpointing (window size "+EnergyEndpointingWindow+" )";
            s+=" absolute with threshold "+AbsoluteEnergyEndpointingThreshold;
        }
        else
        {
            s+="NO ENDPOINTING ALGORITHM!!";
        }
        s+="\n";

        s+="Using "+theCodebook.getName()+"\n";

        if(DISCARDSMALLSYMBOLS)
        {
            s+="Discarding symbols below magnitude "+DiscardMagnitude+"\n";
        }

        s+="Taking final statistic ";
        if(WHICHSTATISTIC==AMATRICES) s+="A Matrix";
        else if (WHICHSTATISTIC==SMATRIXGRAM) s+="SMatrixGram ("+slices+")";
        else throw new IllegalStateException("Haven't set the constant which controls which statistic to use.");
        s+="\n";

        return s;
    }


    public Statistic getStatistic(SoundWave soundwave,List titles,List components) {
        displaySoundWave( soundwave, titles, components);
        if (LOWPASSFILTER) soundwave=LowPassFilter(soundwave, titles, components);
        if (HIGHPASSFILTER) soundwave=HighPassFilter(soundwave, titles, components);
        soundwave=EndPoint(soundwave, titles, components);
        TesparizedWave tw=Tesparize(soundwave, titles, components);
        Statistic s=TesparizedWave2Statistic(tw, titles, components);
        return s;
    }
    
    private static void displaySoundWave(SoundWave soundwave, List titles, List components)
    {
        if(DISPLAYWAVES){
            titles.add("Waves as read");
            components.add(soundwave.getDisplay());
        }
    }
    
    private static SoundWave LowPassFilter(SoundWave s,List titles, List components)
    {
        s=s.filterButterworthLowPassOrder8();
        if(DISPLAYLOWPASSEDWAVES){
            titles.add("Waves after low pass filter");
            components.add(s.getDisplay());
        }
        return s;
    }

    private static SoundWave HighPassFilter(SoundWave s, List titles, List components)
    {
        s=s.filterButterworthHighPassOrder8();
        if(DISPLAYHIGHPASSEDWAVES){
            titles.add("Waves after high pass filter");
            components.add(s.getDisplay());
        }
        return s;
    }
    
   private static SoundWave EndPoint(SoundWave s,List titles, List components)
   {
        
        if(DISPLAYPREENDPOINTINGENERGY)
        {
            titles.add("Energy before endpointing");
            components.add(s.getEnergyDisplay(EnergyEndpointingWindow));
        }
        
        if(RELATIVEENERGYENDPOINTING){
            s=s.endpointRelative(EnergyEndpointingWindow, RelativeEnergyEndpointingThreshold);
        }
        else if(ABSOLUTEENERGYENDPOINTING)
        {
            s=s.endpointAbsolute(EnergyEndpointingWindow, AbsoluteEnergyEndpointingThreshold);
        }

        if(DISPLAYENDPOINTEDWAVES){
            titles.add("After endpointing");
            components.add(s.getDisplay());
        }
        
        if(DISPLAYPOSTENDPOINTINGENERGY)
        {
            titles.add("Energy after endpointing");
            components.add(s.getEnergyDisplay(EnergyEndpointingWindow));
        }

        return s;
    }

    
    private static TesparizedWave Tesparize(SoundWave soundwave, List titles, List components)
    {
        TesparizedWave tw=new TesparizedWave(soundwave);
        if(DISPLAYTESPARIZEDWAVES){
            titles.add("Tesparized Waves");
            components.add(tw.getDisplay());
        }
        if(DISPLAYENCODEDWAVES){
            titles.add("Codebook Symbols");
            components.add(tw.getCodedDisplay(theCodebook));
        }
        if(DISPLAYSLIDINGINNERPRODUCTS){
            titles.add("Sliding Inner Product After Coding");
            components.add(tw.getSlidingInnerProductDisplay(theCodebook, 1000));
         }

        return tw;
    }


     private static Statistic TesparizedWave2Statistic(TesparizedWave tw, List titles, List components)
    {
        Statistic s;

        if(WHICHSTATISTIC==AMATRICES)
        {
            s=new AMatrix(tw, theCodebook);
        }
        else if (WHICHSTATISTIC==SMATRIXGRAM)
        {
            s=new SMatrixGram(tw, theCodebook, slices, true);
            if(DISPLAYINTERNALANGLES) {
                titles.add("Internal Angles");
                components.add(((SMatrixGram)s).getInternalAnglesDisplay());
            }
        }
        else throw new IllegalStateException("Haven't set the constant which controls which statistic to use.");

        if(DISPLAYSTATISTICS) {
            titles.add(s.getStatisticType());
            components.add(s.visualise());
        }
        return s;
    }

    public static void main(String[] args) throws IOException
    {
        DefaultStrategy d=new DefaultStrategy();
        d.getAndDisplayStatistic(new SoundWave(new java.io.File("C:\\speech databases\\livejohndigitssony\\record01\\6.txt"), 16000), "messages");
        d.getAndDisplayStatistic(new SoundWave(new java.io.File("C:\\ddldata\\mac_n0s3d6.wav")), "six");
        d.getDescription();
        
    }
}