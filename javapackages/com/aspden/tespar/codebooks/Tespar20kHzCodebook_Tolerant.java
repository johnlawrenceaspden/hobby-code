/*
 *  Tespar20kHzCodebookWithSilenceSymbol_Tolerant.java
 */

package com.aspden.tespar.codebooks;
import com.aspden.tespar.*;
import com.aspden.tespar.basics.*;

/** This is the traditional TESPAR codebook for 20kHz sampling. It is thus possibly not appropriate for our 16kHz waves and should be scaled.
 * It is tolerant in the sense that epochs with large shape relative to their duration are not treated as exceptional, but given an appropriate symbol for their duration.
 * Symbol 0 is the for low magnitude epochs (silence).
 * Symbol 29 is for those which go off the scale in terms of duration.
 */
public class Tespar20kHzCodebook_Tolerant implements Codebook {
    int silenceThreshold;
    private static final int SILENCE=0;
    private static final int OUTSIDECODEBOOK=29;

    /** Creates new TESPAR20kHzCodebookWithSilenceSymbol_Tolerant.
     * @param silenceThreshold The smallest magnitude epoch which is not to be considered as silence.
     */
    public Tespar20kHzCodebook_Tolerant(int silenceThreshold) {
        this.silenceThreshold=silenceThreshold;
    }

    public String getName()
    {
        return "Tolerant Tespar 20kHz Codebook with Silence Symbol (Silence Threshold "+this.silenceThreshold+" )";
    }

    public int getSymbol(int duration, int shape, double magnitude)
    {
        if(duration==0) throw new IllegalStateException("Symbol with duration 0! Should not occur");

        if (magnitude<silenceThreshold) return 0; //Silence symbol

        if(duration<=5)
        {
            return duration;
        }
        else if(duration<=7)
        {
            return 6;
        }
        else if(duration<=10)
        {
            switch(shape)
            {
                case 0: return 7;
                case 1: return 8;
                default: return 8;
            }
        }
        else if(duration<=13)
        {
            switch(shape)
            {
                case 0: return 9;
                case 1: return 10;
                default: return 10;
            }
        }
        else if(duration<=18)
        {
            switch(shape)
            {
                case 0: return 11;
                case 1: return 12;
                case 2: return 13;
                default: return 13;
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
                default: return 17;
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
                default: return 22;
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
                default: return 28;
            }
        }
        else return OUTSIDECODEBOOK;
    }

    public int getCodebookSize()
    {
        return OUTSIDECODEBOOK+1;
    }
}