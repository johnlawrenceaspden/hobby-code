/*
 * StatComparator.java
 */

package com.aspden.tespar.statscomparison;
import com.aspden.tespar.statistics.*;

/** A Scorer is created with a list of Archetypes,
 * and given a statistic returns its score against each of those Archetypes.
 */
public interface StatComparator {
    /** Given a Statistic return an array of scores.
     * @param s The Statistic to be tested.
     * @return The score of the statistic against each of the entities against which it has been compared.
     */
    public double[] getScores(Statistic s);
    public StatComparator getInstance(Archetype[] theArchetypes);
}
