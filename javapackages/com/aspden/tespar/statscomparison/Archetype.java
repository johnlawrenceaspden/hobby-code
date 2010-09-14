/*
 * Archetype.java
 */

package com.aspden.tespar.statscomparison;

import java.util.*;
import com.aspden.tespar.statistics.*;


/** An Archetype is a collection of Statistics, such as A or S Matrices,  all of the same kind, 
 * to which other Statistics of the same kind may be compared.
 */
public class Archetype extends Object {
  List theList=new ArrayList();

  /** Add a new Statistic to the Archetype.
   * @param a the new Statistic.
   */
  public void add(Statistic a) {
    theList.add(a);
  }
  
  /** Takes the 'average' (as defined by the Statistic classes) of all the Statistics in the Archetype.
   * @return The 'average' Statistic in the Archetype.
   */
  public Statistic asAverage() {
    if(theList.isEmpty()) throw new IllegalStateException("Can't take the average of an empty archetype");

    //need to call the correct asAverageMethod so must get dummy Statistic off the list and use it to call function.
    Statistic s=(Statistic) theList.get(0);
    return (s.asAverage(theList));
  }
}