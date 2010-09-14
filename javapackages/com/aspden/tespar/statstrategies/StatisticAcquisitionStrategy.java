/*
 * StatisticAcquisitionStrategy.java
 */

package com.aspden.tespar.statstrategies;

import com.aspden.tespar.statistics.*;
import com.aspden.tespar.basics.*;
import com.aspden.graph.*;

import java.util.List; //use util.List not awt.List
import java.util.*;
import java.awt.*;

/** Abstract base class for methods of turning sound wave files into TESPAR statistics.
 */
public abstract class StatisticAcquisitionStrategy extends Object {

  /** Process a sound file to produce a TESPAR Statistic and (perhaps) various graphics.
   * During the processing, a Strategy may produce various informative graphical components illustrating the processing that has gone on.
   * These and their titles will be added to the two supplied lists.
   * @param filename The file to process.
   * @param titles List of picture titles
   * @param components List of pictures produced during processing. All will be AWT components.
   * @return The Statistic derived from the file by the strategy.
   */
  public abstract Statistic getStatistic(SoundWave wave, List titles, List components);

  /** Process a sound file to produce a TESPAR Statistic.
   * A wrapper for the more complicated version of getStatistic which relieves
   * the caller of the necessity to discard any graphical components which may be
   * produced in processing.
   * @param filename The sound file to process.
   * @return The Statistic derived from the file
   */
  public Statistic getStatistic(SoundWave wave) {
    Vector dummy1=new Vector();
    Vector dummy2=new Vector();
    return getStatistic(wave, dummy1, dummy2);
  }


  /** Process a sound file to produce a TESPAR Statistic.
   * A wrapper for the more complicated version of getStatistic which displays the graphics (if any) itself.
   * @param filename The sound file to process.
   * @return The Statistic derived from the file
   */
  public Statistic getAndDisplayStatistic(SoundWave wave, String title)
  {
    Vector titles=new Vector();
    Vector components=new Vector();
    Statistic stat = getStatistic(wave, titles, components);
    
    if(titles.size()>0)
    {
      String[] stitles = (String[]) titles.toArray(new String[titles.size()]);

      ButtonedTableDisplayer table = new ButtonedTableDisplayer(stitles,new String[]{""});

      Iterator icomponents=components.iterator();
      int i=0;
      while(icomponents.hasNext())
      {
        Component c=(Component) icomponents.next();
        table.add(c, i++, 0);
      }
      new SingleComponentFrame(table,title);
    }

    return stat;
  }





  /** Get a text description of the strategy.
   * @return Detailed description of the method this strategy will use to turn sound into TESPAR Statistics.
   */
  public abstract String getDescription();
}