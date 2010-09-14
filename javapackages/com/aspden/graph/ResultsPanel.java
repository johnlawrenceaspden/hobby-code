/*
 * ResultsPanel.java
 */

package com.aspden.graph;

import com.aspden.graph.*;
import com.aspden.*;

import java.awt.*;
import java.text.*;

/**
 * A table of scores with row and column labels, with backgrounds coloured according to score value.
 */
public class ResultsPanel extends Panel {
  
  private void constructorCommonCode(String[] rows, String[] columns, double[][] scores, double brightest, double darkest)
  {
    if(rows.length!=scores.length) throw new IllegalArgumentException("row labels != rows of scores");
    
    NumberFormat f=new DecimalFormat("##");
    PlainTableDisplayPanel t =new PlainTableDisplayPanel(rows, columns);
    for(int j=0; j<rows.length; j++)
    {
      for(int i=0; i<columns.length; i++)
      {
        if(columns.length!=scores[j].length) throw new IllegalArgumentException("column labels != columns of scores in row"+j);
        Component l=new Label(f.format(scores[j][i]));
        Color c;
        //each score's background should be a red shade dependent on the brightest-darkest interval. Scores outside the interval are green.
        double normalizedscore=(scores[j][i]-darkest)/(brightest-darkest);
        if(normalizedscore>1.0) c=Color.green;
        else if(normalizedscore< 0.0) c=Color.green;
        else c=new Color((int)(255*normalizedscore),0,0);
        
        l.setBackground(c);
        t.add(l,j,i);
      }
    }
    this.setLayout(new BorderLayout());
    this.add(t);
  }

  /** Creates a score table with only one row. A convenience constructor.
 * @param scores the row of scores
 * @param row the label for the row
 * @param columns the column labels
 * @param brightest the score which is to be brightest
 * @param darkest the score which is to be darkest
 */
  public ResultsPanel(String row, String[] columns, double[] scores, double brightest, double darkest) {
    constructorCommonCode(new String[]{row}, columns, new double[][]{scores}, brightest, darkest);
  }

  /** Creates new table of scores
 * @param scores the scores as an array s[row][column]
 * @param rows labels for the rows
 * @param columns labels for the columns
 * @param brightest the score which is to be brightest
 * @param darkest the score which is to be darkest
 */
  public ResultsPanel(String[] rows, String[] columns, double[][] scores, double brightest, double darkest)
  {
    constructorCommonCode(rows, columns, scores, brightest, darkest);
  }

  /** Creates new table of scores
 * @param scores the scores as an array s[row][column]
 * @param rows labels for the rows
 * @param columns labels for the columns
 * @param brightest the score which is to be brightest
 * @param darkest the score which is to be darkest
 */
  public ResultsPanel(String[] rows, String[] columns, int[][] scores, double brightest, double darkest)
  {
     constructorCommonCode(rows, columns, Stuff.int2Double(scores), brightest, darkest);
  }



  /** Test/Demo code
   * @param args ignored.
   */
  public static void main (String args[]) {
    String[] teams={"aardvarks", "penguins"};
    String[] rounds={"round 1","round 2","round 3"};
    double[] scores={22,34.56789};
    double[][] scores2={{23, 101}, {3, 45}, {93, 27}};
    new SingleComponentFrame(new ResultsPanel("overall", teams, scores,  0, 100), "Animal Championship End of Season Scores") ;
    new SingleComponentFrame(new ResultsPanel( rounds,   teams, scores2, 0, 100), "Animal Championship Table");
  }

}