/*
 * MovingWaveDisplayer.java
 */

package com.aspden.graph;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;


/** A hospital heart monitor type display.
 * A MovingWaveDisplayer allows the display of multiple independently scaled moving traces.
 * Data can be added to each trace independently and the display will update in real time.
 */
public class MovingWaveDisplayer extends Panel {

    private int no; //The number of traces to display.
    private int length; //the maximum length of the traces.
    private double [][] vals; //The values in the wave forms which are to be displayed.
    private int[] writePosition; //The position reached in each wave form.
    private double [] yMin, yMax; //The ranges for each.
    private double xsc;
    private double[] ysc;
    private Color[] colour;
    private Graphics theGraphics;
    private Image theImage;

    /** Constructs a component to display a certain number of moving traces using specified scales and colours for each trace.
     * @param length the maximum length of the traces to be supplied. (Need to know to scale the display.)
     * @param no the number of traces to be displayed
     * @param ymin array of minimum values (for scaling purposes. One for each trace in order.)
     * @param ymax array of maximum values
     * @param colour colour for each trace.
     */
    public MovingWaveDisplayer(int length,int no, double[] ymin, double[] ymax, Color[] colour) {
        enableEvents(AWTEvent.COMPONENT_EVENT_MASK); //Need to process resize events to keep the scales the same size as the screen.

        //Now we know how many arrays to deal with we can initialise them all.
        this.no=no;
        this.length=length;
        vals=new double[no][];
        writePosition=new int[no];
        yMin=new double[no];
        yMax=new double[no];
        ysc=new double[no];
        this.colour=new Color[no];
        for(int i=0; i<no; i++)
        {
            vals[i]=new double[length];
            writePosition[i]=0;
            this.yMin[i]=ymin[i];
            this.yMax[i]=ymax[i];
            this.colour[i]=colour[i];
        }
    }


    /** Overridden in order to catch resize and show events.
     * Must recalculate scales when this happens.
     * @param e Operating system event.
     */
    protected void processComponentEvent(ComponentEvent e) 
    {
        {
            Dimension d=this.getSize();
            for(int i=0; i<no; i++)
            {
                xsc=d.width/(double)length;
                ysc[i]=d.height/(yMax[i]-yMin[i]);
            }
        }
        if(theGraphics!=null) theGraphics.dispose();
        theGraphics=this.getGraphics();
        super.processComponentEvent(e);
    }

    public void paint(Graphics g) {
        Image theImage=createImage(this.getWidth(), this.getHeight());
        drawAll(theImage.getGraphics());
        g.drawImage(theImage, 0,0,this);
    }

    private void drawLine(int trace, int x1,double y1, int x2, double y2, Graphics g)
    {
        if(g==null) return;
           g.setColor(colour[trace]);
           g.drawLine((int)(x1*xsc), (int)((yMax[trace]-y1)*ysc[trace]), (int)(x2*xsc),(int) ((yMax[trace]-y2)*ysc[trace]));
    }

    private void drawAll(Graphics g)
    {
        g.clearRect(0,0, this.getWidth(), this.getHeight());
        for(int j=0;j<no;j++)
        {
            double[] y=vals[j];
            for(int i=0; i<writePosition[j]-1; i++) drawLine(j,i,y[i],i+1, y[i+1],g);
        }
    }

    /** Add a new value to one of the traces. Causes redisplay with the updated information.
     * @param trace Which trace the new value is to be added to.
     * @param val The value to be added.
     */
    public void add(int trace,double val) {
        if(writePosition[trace]==this.length) return; //run out of space for this trace so throw value away.

        vals[trace][writePosition[trace]++]=val;

        if((writePosition[trace]==1) )
        {
            //nothing to do.
        }
        else
        {
            Graphics g=this.theGraphics;
            drawLine(trace,writePosition[trace]-2,vals[trace][writePosition[trace]-2],writePosition[trace]-1, vals[trace][writePosition[trace]-1],g);
        }
    }

    /** Destroy all the information on the graph, clear the display and start again.
     */
    public void reset() {
        for(int i=0; i<no; i++)
        {
            vals[i]=new double[length];
            writePosition[i]=0;
        }
        repaint();
    }


    /** Demo/Test code. Creates animation of three random walks.
     * @param args the command line arguments. Not used.
     */
    public static void main (String args[]) {
        final int size=1000;
        Random r= new Random();
        JFrame f = new JFrame("Moving Wave Displayer Test");
MovingWaveDisplayer mwd = new MovingWaveDisplayer(size,3,new double[]{0,0,0}, new double[]{size,2*size,4*size}, new Color[]{Color.blue, Color.red, Color.green});
        mwd.setBackground(Color.white);
        f.getContentPane().add(mwd);
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        f.pack();
        f.show();
        double y=size/2;
        double w=size;
        double z=size*2;

        for(;;)
        {
            //try{Thread.sleep(0,1);}catch(Exception e){};
            Thread.yield();
            switch(r.nextInt(3))
            {
                case 0: //deliberate falling-through.
                y+=r.nextInt(21)-10;
                mwd.add(0,y);
                case 1:
                w+=r.nextInt(41)-20;
                mwd.add(1,w);
                case 2:
                z+=r.nextInt(81)-40;
                mwd.add(2,z);
            }
            if(r.nextDouble()<0.0005)
            {
                y=size/2;
                w=size;
                z=size*2;
                mwd.reset();
            }
        }
    }

}