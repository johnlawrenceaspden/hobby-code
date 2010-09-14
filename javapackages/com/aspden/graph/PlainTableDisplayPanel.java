/*
 * PlainTableDisplayPanel.java
 */

package com.aspden.graph;

import java.awt.*;
import com.aspden.graphwidget.misc.*;

public class PlainTableDisplayPanel extends Panel implements TableDisplayer{
  Panel centralPanel, westPanel, northPanel;
  String[] rowlabels, columnlabels;

  /** Creates new TableDisplayer */
  public PlainTableDisplayPanel(String[] rowlabels,String[] columnlabels) {
    this.rowlabels=rowlabels;
    this.columnlabels=columnlabels;
    createCentralPanel();
    createWestPanel();
    createNorthPanel();
    //The best way to figure out what this is for is to run it. A table with labels at the top and left.
    this.setLayout(new GridBagLayout());
    GridBagConstraints c= new GridBagConstraints();
    c.fill=GridBagConstraints.BOTH;
    
    c.gridwidth=columnlabels.length; c.gridheight=1;
    c.gridx=1; c.gridy=0;
    c.weightx=1; c.weighty=0;
    this.add(northPanel, c);
    
    c.gridwidth=1; c.gridheight=rowlabels.length;
    c.gridx=0; c.gridy=1;
    c.weightx=0; c.weighty=1;
    this.add(westPanel, c);
    
    c.gridwidth=columnlabels.length; c.gridheight=rowlabels.length;
    c.gridx=1; c.gridy=1;
    c.weightx=1; c.weighty=1;
    this.add(centralPanel,c);
    
    this.setSize(40*(columnlabels.length+1),40*(rowlabels.length+1));
  }

/** Note that this doesn't work properly due to laziness on my part. You have to add them in order.
 * @param c
 * @param row
 * @param column
 */
  public void add(Component c, int row, int column)
  {
    centralPanel.add(c);
  }

  private void createNorthPanel()
  {
    northPanel=new Panel();
    northPanel.setLayout(new GridLayout(1,columnlabels.length));
    for(int i=0; i<columnlabels.length; i++)
    {
      northPanel.add(new Label(columnlabels[i]));
    }
  }

  private void createWestPanel()
  {
    westPanel=new Panel();
    westPanel.setLayout(new GridLayout(rowlabels.length,1));
    for(int i=0; i<rowlabels.length; i++)
    {
      westPanel.add(new Label(rowlabels[i]));
    }

  }

  private void createCentralPanel()
  {
    centralPanel=new Panel();
    centralPanel.setBackground(Color.black);
    centralPanel.setLayout(new GridLayout(rowlabels.length, columnlabels.length, 1, 1));
  }
  
  public void highlight(int row, int column)
  {
    (centralPanel.getComponents())[row*columnlabels.length+column].setBackground(Color.lightGray);
  }


}