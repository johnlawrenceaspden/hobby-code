/*
 * ClassifierPanel.java
 */

package com.aspden.graph;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

public class ClassifierPanel extends JPanel implements ActionListener{
    
    private Vector theListeners=new Vector();
    private Vector theButtons=new Vector();
    private JButton deleteButton=new JButton("DELETE");
    
    
    /** Creates new ClassifierPanel */
    public ClassifierPanel(Component c, String[] possibles) {
        this.add(c);
        for(int i=0; i<possibles.length; i++)
        {
            JButton b=new JButton(possibles[i]);
            theButtons.add(b);
            b.addActionListener(this);
            this.add(b);
        }
        this.add(deleteButton);
        deleteButton.addActionListener(this);
    }
    
    private void unclassifiable()
    {
        Iterator i=theListeners.iterator();
        while(i.hasNext())
        {
            ((ClassifierListener) i.next()).unclassifiable();
            
        }
    }
    
    private void classified(int j)
    {
        Iterator i=theListeners.iterator();
        while(i.hasNext())
        {
            ((ClassifierListener) i.next()).classified(j);
            
        }
    }
    
    public void addListener(ClassifierListener c)
    {
        this.theListeners.add(c);
    }

    public void actionPerformed(final java.awt.event.ActionEvent e) {
        Object o=e.getSource();
        if(o==deleteButton) unclassifiable();
        else if (theButtons.contains(o))
        {
            classified(theButtons.indexOf(o));
        }
    }
    
    public interface ClassifierListener{
        public void classified(int i);
        public void unclassifiable();
    }

    /**
     * Demo/Test Code
     * @param args ingnored
     */
    public static void main (String args[]) throws java.io.IOException  {
    final String[] names={"alpha","beta","gamma"};
       LookAndFeelStuff.randomise();
        Component c=new TextArea("Thing to be classified goes here");
        ClassifierPanel a=new ClassifierPanel(c, names);
        new SingleComponentFrame(a,"Classifier Test");
        ClassifierListener b=new ClassifierListener(){
            public void classified(int i){
                System.out.println("classified as "+names[i]);
            }
            public void unclassifiable(){

                System.out.println("unclassifiable");
            }
        };
        a.addListener(b);
    }

}