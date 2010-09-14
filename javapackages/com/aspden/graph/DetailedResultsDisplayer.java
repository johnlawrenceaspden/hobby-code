/*
 * DetailedResultsDisplayer.java
 */

package com.aspden.graph;

import com.aspden.graph.*;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import java.util.List;
import javax.swing.*;

/** Allows the display of summarized and detailed scores for a list of candidates.
 * Initially only the summary is visible but it contains a button which will make visible the detailed results.
 */
public class DetailedResultsDisplayer extends Object implements ActionListener{
  JButton detailsButton=new JButton("details");
  Vector subFrames=new Vector();

  /** Creates new DetailedResultsDisplayer
 * @param title The title of the main summary results window
 * @param files
 * @param scores
 * @param theTitles
 * @param theGrabs
 * @param theScores
 * @param winner
 */
  public DetailedResultsDisplayer(String summaryTitle, String summaryRow, String[] candidates, double[] summaryScores, List theSubTableTitles, List theSubTableRows, List theSubTableScores, double brightest, double darkest) {
    Panel p=new Panel();
    p.setLayout(new BorderLayout());
    p.add(new ResultsPanel( summaryRow, candidates, summaryScores, brightest, darkest));
    p.add(detailsButton, BorderLayout.EAST);
    detailsButton.addActionListener(this);
    Frame mainframe=new SingleComponentFrame(p, summaryTitle);

    Iterator iTitles = theSubTableTitles.iterator();
    Iterator iRows = theSubTableRows.iterator();
    Iterator iScores = theSubTableScores.iterator();

    while(iTitles.hasNext())
    {
      Frame f=new Frame((String)iTitles.next());  
      f.add(new ResultsPanel((String[])iRows.next(), candidates, (double[][])iScores.next(), brightest, darkest));
      f.addWindowListener(new WindowAdapter(){public void windowClosing(WindowEvent e){detailsButton.doClick();}});
      f.pack();
      subFrames.add(f);
    }
  }

  public void actionPerformed(final java.awt.event.ActionEvent p1) {
    Iterator i=subFrames.iterator();
    while(i.hasNext())
    {
      Frame f=(Frame) i.next();
      f.setVisible(!(f.isVisible()));
    }
    
  }

  /** Test/Demo code
   * @param args ignored.
   */
  public static void main (String args[]) {
  String[] teams={"aardvarks", "penguins"};
  double[] totalscores={22,34.56789};

    Vector theSeasons=new Vector();
    Vector theRounds=new Vector();
    Vector theScores=new Vector();

    {
      theSeasons.add("1999");
      theRounds.add(new String[]{"round 1", "round 2"});
      theScores.add(new double[][]{{23, 78}, {3, 45}});
    }

    {
      theSeasons.add("2000");
      theRounds.add(new String[]{"round 1", "round 2"});
      theScores.add(new double[][]{{32, 21}, {5, 23}});
    }

    new DetailedResultsDisplayer("Animal Championship", "aardvarks!", teams, totalscores, theSeasons, theRounds, theScores, 0, 90);
  }

}