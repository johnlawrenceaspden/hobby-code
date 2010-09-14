/*
 * CorrelateAgainstAverageComparator.java
 */

package com.aspden.tespar.statscomparison;

import com.aspden.tespar.statistics.*;

/** Comparision using the Statistics' natural correlation against the Archetypes' natural average.
 */
public class CorrelateAgainstAverageComparator extends Object implements StatComparator{
    Statistic[] AveragesOfArchetypes;

    public CorrelateAgainstAverageComparator(Archetype[] theArchetypes)
    {

        AveragesOfArchetypes=new Statistic[theArchetypes.length];
        for(int i=0; i<theArchetypes.length; i++)
        {
            AveragesOfArchetypes[i] = theArchetypes[i].asAverage();
        }
    }

    public StatComparator getInstance(Archetype[] theArchetypes)
    {
        return new CorrelateAgainstAverageComparator(theArchetypes);
    }

    public double[] getScores(Statistic s)
    {
        double[] scores=new double[AveragesOfArchetypes.length];
        for(int i=0; i<AveragesOfArchetypes.length; i++)
        {
            scores[i]=s.correlate(AveragesOfArchetypes[i]);
        }
        return scores;
    }
}
