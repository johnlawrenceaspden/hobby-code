/*
 * Corpus.java
 */

package com.aspden.tespar;

import com.aspden.tespar.statstrategies.*;
import com.aspden.tespar.statscomparison.*;
import com.aspden.tespar.statistics.*;
import com.aspden.tespar.basics.*;
import com.aspden.tespar.utility.*;
import com.aspden.soundrecorder.*;
import com.aspden.graph.*;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.io.*;

/** A big collection of calculated statistics against which single statistics can be compared.
 */
public class Corpus extends Object{
    private StatisticAcquisitionStrategy theStrategy;
    private String[] filenames;
    private Collection theTables=new Vector();
    private Archetype[] theArchetypes;
    private boolean detailedDisplays;
    private int frequency;

    /** Creates new Corpus*/
    public Corpus(StatisticAcquisitionStrategy theStrategy, boolean detailedDisplays) {
        this.theStrategy=theStrategy;
        this.detailedDisplays=detailedDisplays;
    }

    public void add(SpeechDatabase s) throws IOException
    {
        if(theArchetypes==null) //if it's the first database set things up.
        {
            this.filenames=s.getCuteNames();
            this.theArchetypes=new Archetype[s.getFiles().length];
            for(int i=0; i<this.filenames.length; i++) theArchetypes[i]=new Archetype();
            this.frequency=s.getFrequency();
        }
        else
        {
            if (s.getFiles().length!=this.theArchetypes.length) throw new IllegalArgumentException("Incompatible Databases");
            if (s.getFrequency()!=frequency) throw new IllegalArgumentException("Incompatible Databases");
        }

        StatisticsTable stattable=new StatisticsTable(s.getRoot(), s.getRoot(), s.getSubdirs(), s.getFiles(), s.getFrequency(), this.theStrategy, detailedDisplays);
        for(int i=0; i<s.getFiles().length; i++)
        {
            for(int j=0; j<s.getSubdirs().length; j++)
            {
                theArchetypes[i].add(stattable.getStatistic(j,i));
            }
        }
        theTables.add(stattable);
    }

    public String compare(SoundWave soundwave, String name)
    {
        double[] totalscores;
        Statistic stat;

        if(detailedDisplays){
            stat=theStrategy.getAndDisplayStatistic(soundwave, name);
        } else {
            stat=theStrategy.getStatistic(soundwave);
        }

        if(theArchetypes==null) throw new IllegalStateException("Can't compare vs. empty corpus");
       
        StatComparator sc=new CorrelateAgainstAverageComparator(theArchetypes);
        totalscores = sc.getScores(stat);


        int winner=Mathematics.lowest(totalscores);


        if(detailedDisplays) {
            Vector theTitles=new Vector();
            Vector theGrabs=new Vector();
            Vector theScores=new Vector();

            Iterator itable = theTables.iterator();
            while(itable.hasNext())
            {
                StatisticsTable st=(StatisticsTable)(itable.next());
                double[][] scores = st.compare(stat);

                theTitles.add(name+" vs. "+st.getName());
                theGrabs.add(st.getDirectories());
                theScores.add(scores);
            }
            new DetailedResultsDisplayer(name, this.filenames[winner], this.filenames, totalscores, theTitles, theGrabs, theScores, 0, 90);
        }

        return this.filenames[winner];


    }


    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) throws Exception {
        
        Corpus theCorpus = new Corpus(new DefaultStrategy(), false);
        theCorpus.add(SpeechDatabase.MoreLiveJohnDigitsSony);
        theCorpus.compare(new SoundWave(new File(SpeechDatabase.TestFile),SpeechDatabase.TestFreq), "test");
    }

}
