/*
 * ButtonedTableDisplayer.java
 */

package com.aspden.graph;

import java.awt.*;
import java.awt.event.*;
import com.aspden.graphwidget.misc.*;

/**A table of components arranged by file and directory. Clicking on the labels/buttons makes rows and columns disappear.*/
public class ButtonedTableDisplayer extends Panel implements TableDisplayer
{
  Panel centralPanel=new Panel(), westPanel=new Panel(), northPanel=new Panel();
  String[] directories, filenames;
  Component[][] theComponents;
  Button[] directoryButtons;
  Button[] fileButtons;
  boolean directoryDisplay[];
  boolean fileDisplay[];
  Button restoreButton;

  public ButtonedTableDisplayer(String[] directories,String[] filenames)
  {
    this.directories=directories;
    this.filenames=filenames;
    directoryDisplay=new boolean[directories.length]; for(int i=0; i<directories.length; i++) directoryDisplay[i]=true;
    fileDisplay=new boolean[filenames.length];        for(int i=0; i<filenames.length; i++) fileDisplay[i]=true;
    theComponents=new Component[directories.length][filenames.length];
    createButtons();
    arrangeAll();
  }

  public void highlight(int dir, int file)
  {
    theComponents[dir][file].setBackground(Color.lightGray);
    theComponents[dir][file].repaint();
  }

  private void arrangeAll()
  {
    arrangeCentralPanel();
    arrangeWestPanel();
    arrangeNorthPanel();
    arrangeFrame();
    this.validate();
  }

  /**The best way to figure out what this is for is to run it. A table with labels at the top and left.*/
  private void arrangeFrame()
  {
    this.removeAll();
    this.setLayout(new GridBagLayout());
    GridBagConstraints c= new GridBagConstraints();
    c.fill=GridBagConstraints.BOTH;

    c.gridwidth=1; c.gridheight=1;
    c.gridx=0; c.gridy=0;
    c.weightx=0; c.weighty=0;
    this.add(restoreButton, c);


    c.gridwidth=filenames.length; c.gridheight=1;
    c.gridx=1; c.gridy=0;
    c.weightx=1; c.weighty=0;
    this.add(northPanel, c);

    c.gridwidth=1; c.gridheight=directories.length;
    c.gridx=0; c.gridy=1;
    c.weightx=0; c.weighty=1;
    this.add(westPanel, c);

    c.gridwidth=filenames.length; c.gridheight=directories.length;
    c.gridx=1; c.gridy=1;
    c.weightx=1; c.weighty=1;
    this.add(centralPanel,c);

  }

  private void createButtons()
  {
    restoreButton=new Button("Display all");
    restoreButton.addActionListener(new RestoreAll());

    fileButtons=new Button[filenames.length];
    directoryButtons=new Button[directories.length];
    for(int i=0; i<filenames.length; i++)
    {
      fileButtons[i]=new Button(filenames[i]);
      fileButtons[i].addActionListener(new BooleanChanger(fileDisplay,i));
    }
    for(int i=0; i<directories.length; i++)
    {
      directoryButtons[i]=new Button(directories[i]);
      directoryButtons[i].addActionListener(new BooleanChanger(directoryDisplay,i));
    }
  }

  
  public void add(Component c, int directory, int filename)
  {
    theComponents[directory][filename]=c;
    arrangeCentralPanel();
  }

  private void arrangeNorthPanel()
  {
    northPanel.removeAll();
    northPanel.setLayout(new GridLayout(1,getNumberofTrue(fileDisplay)));
    for(int i=0; i<filenames.length; i++)

    {
      if(fileDisplay[i]) northPanel.add(fileButtons[i]);
    }
  }

  private void arrangeWestPanel()
  {
    westPanel.removeAll();
    westPanel.setLayout(new GridLayout(getNumberofTrue(directoryDisplay),1));
    for(int i=0; i<directories.length; i++)

    {
      if (directoryDisplay[i]) westPanel.add(directoryButtons[i]);
    }

  }

  private void arrangeCentralPanel()
  {
    centralPanel.removeAll();
    centralPanel.setBackground(Color.black);
    if(getNumberofTrue(directoryDisplay)==0 && getNumberofTrue(fileDisplay)==0) return;
    centralPanel.setLayout(new GridLayout(getNumberofTrue(directoryDisplay), getNumberofTrue(fileDisplay), 1, 1));
    for(int dir=0; dir<directories.length; dir++)

    {
      if(!directoryDisplay[dir]) continue;
      for(int file=0; file<filenames.length; file++)

      {
        if(!fileDisplay[file]) continue;
        Component c=theComponents[dir][file];
        if(c==null) centralPanel.add(new Panel());
        else centralPanel.add(c);
      }
    }
  }

  private int getNumberofTrue(boolean[] b)
  {
    int count=0;
    for(int i=0; i<b.length; i++) if(b[i]) count++;
    return count;
  }

  class RestoreAll implements ActionListener
  {
    public void actionPerformed(ActionEvent e)
    {
      for(int i=0; i<directories.length; i++) directoryDisplay[i]=true;
      for(int i=0; i<filenames.length; i++) fileDisplay[i]=true;
      arrangeAll();
    }
  }

  /**This class ensures that when a button is clicked, all the elements
  other than that associated with the button disappear. It does this by setting all the other members of the boolean 'display' array to be false.
  One is associated with each button, and is told on construction which array and which element of that array it is associated with.*/
  class BooleanChanger implements ActionListener
  {
    boolean[] theArray;
    int theIndex;
    public BooleanChanger(boolean[] array, int index)
    {
      this.theArray=array;
      this.theIndex=index;
    }
    public void actionPerformed(ActionEvent e)
    {
      for(int i=0; i<theArray.length; i++)
      {
        theArray[i]=false;
      }
      theArray[theIndex]=true;
      /*
      theArray[theIndex]=!(theArray[theIndex]);
       */

      arrangeAll();
    }
  }

}

