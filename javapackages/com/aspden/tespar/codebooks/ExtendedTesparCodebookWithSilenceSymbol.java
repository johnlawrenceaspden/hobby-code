/*
 *  ExtendedTesparCodebookWithSilenceSymbol.java
 */
 
package com.aspden.tespar.codebooks;

import com.aspden.tespar.*;
import com.aspden.tespar.basics.*;


/** This is the traditional TESPAR codebook for 20kHz sampling, but with extra symbols to deal with longer durations than usual. Symbol 0 is the symbol for low-magnitude natural pairs. The highest symbol is for those which go off the (extended) scale in terms of duration and shape.
 *
 */
public class ExtendedTesparCodebookWithSilenceSymbol implements Codebook {
  int silenceThreshold;
  private static final int SILENCE=0;
  private static final int OUTSIDECODEBOOK=36;

/** Create a new Codebook, specifying the silence threshold below which small symbols are to be treated as the silence symbol 0.
 * @param silenceThreshold smallest magnitude that is not considered to be silence.
 */
  public ExtendedTesparCodebookWithSilenceSymbol(int silenceThreshold) {
    this.silenceThreshold=silenceThreshold;
  }
  
  public String getName()
  {
    return "Extended Tespar Codebook with Silence Symbol (Silence Threshold "+this.silenceThreshold+" )";
  }

  public int getSymbol(int duration, int shape, double magnitude)
  {
   
    if(duration==0) throw new IllegalArgumentException("Symbol with duration 0! Should not occur");
    
    if (magnitude<silenceThreshold) return 0; //Silence symbol
    
    if(duration<=5)
    {
      if(shape>0) return OUTSIDECODEBOOK;
      else return duration;
    }
    else if(duration<=7) 
    {
      if(shape>0) return OUTSIDECODEBOOK;
      else return 6;
    }
    else if(duration<=10)
    {
      switch(shape)
      {
        case 0: return 7;
        case 1: return 8;
        default: return OUTSIDECODEBOOK;
      }
    }
    else if(duration<=13) 
    {
      switch(shape)
      {
        case 0: return 9;
        case 1: return 10;
        default: return OUTSIDECODEBOOK;
      }
    }
    else if(duration<=18) 
    {
      switch(shape)
      {
        case 0: return 11;
        case 1: return 12;
        case 2: return 13;
        default: return OUTSIDECODEBOOK;
      }
    }
    else if(duration<=23) 
    {
      switch(shape)
      {
        case 0: return 14;
        case 1: return 15;
        case 2: return 16;
        case 3: return 17;
        default: return OUTSIDECODEBOOK;
      }
    }
    else if(duration<=30) 
    {
      switch(shape)
      {
        case 0: return 18;
        case 1: return 19;
        case 2: return 20;
        case 3: return 21;
        case 4: return 22;
        default: return OUTSIDECODEBOOK;
      }
    }
    else if(duration<=37) 
    {
      switch(shape)
      {
        case 0: return 23;
        case 1: return 24;
        case 2: return 25;
        case 3: return 26;
        case 4: return 27;
        case 5: return 28;
        default: return OUTSIDECODEBOOK;
      }
    }
    else if(duration<=45)
    {
      switch(shape)
      {
        case 0: return 29;
        case 1: return 30;
        case 2: return 31;
        case 3: return 32;
        case 4: return 33;
        case 5: return 34;
        case 6: return 35;
        default: return OUTSIDECODEBOOK;
      }
    }
    else return OUTSIDECODEBOOK;
  }
  
  public int getCodebookSize()
  {
    return OUTSIDECODEBOOK+1;
  }
  
  
}