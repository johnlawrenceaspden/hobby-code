/*
 * ResultsDisplayer.java
 */
 
package com.aspden.tespar;

import com.aspden.tespar.utility.*;

import java.awt.*;

public class ResultsDisplayer extends Object {
  TextArea theArea=new TextArea();
  

  /** Creates new ResultsDisplayer */
  public ResultsDisplayer() {
    theArea.setFont(new Font("Helvetica", Font.PLAIN, 14));
    java.awt.Frame f=new com.aspden.graphwidget.misc.CloseableFrame("Final results");
    f.add(theArea);
    f.pack();
    f.show();
  }
  
  public void append(ScoreForSet total, String[] directories)
  {
    String results;
    results=total+"     on data ";
    for(int i=0; i<directories.length; i++) results=results+directories[i]+" ";

    theArea.append(results);
    theArea.append("\n");
    
    System.out.println(results);
  }
  
}


