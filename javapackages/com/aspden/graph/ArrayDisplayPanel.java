/*
 * ArrayDisplayPanel.java
 */

package com.aspden.graph;

import com.aspden.graphwidget.graphpanel.array.*;
import com.aspden.graphwidget.linestyles.*;

import java.util.*;



/** An AWT panel which allows for the zoomable display of one or two arrays of doubles, relying on the
 * {@link com.aspden.graphwidget} classes for the display. Drawing a box zooms the graph.
 * One array is on the left axis in blue, the second is on the right axis in red.
 */
public class ArrayDisplayPanel extends java.awt.Panel implements ArrayRangeSelectionListener{
    private ArrayGraphPanel theArrayGraphPanel;
    private List theLData=new ArrayList();
    private List theRData=new ArrayList();
    private Vector theRightClickListeners = new Vector();
    private boolean lautoscale;
    private double  lymin, lymax;
    private boolean rautoscale;
    private double  rymin, rymax;
    
    
    /**private constructor creates the panel but does not add the data. Exists only to be called by the public constructors*/
    private ArrayDisplayPanel()
    {
        theArrayGraphPanel = new ArrayGraphPanel();
        this.setLayout(new java.awt.BorderLayout());
        this.add(theArrayGraphPanel);
        theArrayGraphPanel.addRangeListener(this);
        theArrayGraphPanel.setArrayLabelGenerator( new Labeller());
        theArrayGraphPanel.setLGrid(true);
        this.setBackground(java.awt.Color.white);
    }
    
    /** Display a single graph which will scale to fit the data exactly.
     * @param data The data to be displayed.
     */
    public ArrayDisplayPanel(double[] data)
    {
        this();
        theLData.add(dataclone(data));
        lautoscale=true;
        displayAll();
    }
    
    /** Display a single graph with a given y-scale.
     * @param data data to be displayed.
     * @param ymin y scale to be forced.
     * @param ymax yscale to be forced.
     */
    public ArrayDisplayPanel(double[] data, double ymin, double ymax)
    {
        this();
        theLData.add(dataclone(data));
        lautoscale=false;
        lymin=ymin;
        lymax=ymax;
        displayAll();
    }
    
    /** Display two independently scaled sets of data.
     * They will both be scaled to fit the graph exactly and have their own axis and mouse pointer coordinates.
     * @param ldata blue data on left axis.
     * @param rdata red data on right axis.
     */
    public ArrayDisplayPanel(double[] ldata,double[] rdata)
    {
        this();
        theLData.add(dataclone(ldata));
        theRData.add(dataclone(rdata));
        lautoscale=true;
        rautoscale=true;
        displayAll();
    }
    
    /** Display two sets of data with given y-scales.
     * They will both have their own axis and mouse pointer coordinates.
     * @param ldata blue data on left axis
     * @param lymin forced scale for blue/left
     * @param lymax forced scale for blue/left
     * @param rdata red data on right axis
     * @param rymin forced scale for red/right
     * @param rymax forced scale for red/right
     */
    public ArrayDisplayPanel(double[] ldata, double lymin, double lymax ,double[] rdata, double rymin, double rymax)
    {
        this();
        theLData.add(dataclone(ldata));
        theRData.add(dataclone(rdata));
        lautoscale=false;
        this.lymin=lymin;
        this.lymax=lymax;
        rautoscale=false;
        this.rymin=rymin;
        this.rymax=rymax;
        displayAll();
    }
    
    private void displayAll()
    {
        rangeSelected(0,Integer.MAX_VALUE);
    }
    
    /** Catch range selection events from the the underlying {@link ArrayGraphPanel}
     * and zoom by redisplaying shortened copies of the arrays.
     * @param a selected range
     * @param b selected range
     */
    public void rangeSelected(int a, int b)
    {
        int max=maxLength();
        
        //drawing tiny selection or clicking a point causes unzoom
        if (a==b) {
            a=0; b=max-1;
        }
        
        //drawing the box the wrong way is fine.
        if (a>b) {int c=a; a=b; b=c;}
        
        //don't let the selection go outside the range.
        if (a<0) a=0;
        if (b>max-1) b=max-1;
        
        List LDataArrays=new ArrayList();
        for(Iterator di=theLData.iterator();di.hasNext(); )
        {
            double[] data=(double[]) di.next();
            double[] subrange=new double[b-a+1] ;
            for(int i=a; i<=b; i++) subrange[i-a]=data[i];
            LDataArrays.add(new DataArray(a, subrange, new SimpleLineStyle(java.awt.Color.blue)));
        }
        
        List RDataArrays=new ArrayList();
        for(Iterator di=theRData.iterator();di.hasNext(); )
        {
            double[] data=(double[]) di.next();
            double[] subrange=new double[b-a+1] ;
            for(int i=a; i<=b; i++) subrange[i-a]=data[i];
            RDataArrays.add(new DataArray(a, subrange, new SimpleLineStyle(java.awt.Color.red)));
        }
        
        DisplayIt(LDataArrays, RDataArrays);
    }
    
    /** Usual listener method for listening for right clicks.
     * @param a listening object
     */
    public void addRightClickListener(RightClickListener a)
    {
        theRightClickListeners.add(a);
    }
    
    /** Called by underlying graph.
     * Distribute right click events.
     */
    public void rightButtonReleased()
    {
        Iterator i=theRightClickListeners.iterator();
        while(i.hasNext())
        {
            ((RightClickListener)(i.next())).rightClick();
        }
    }
    
    private void DisplayIt(List lDataArrays, List rDataArrays)
    {
        theArrayGraphPanel.clearData();
        for(Iterator i=lDataArrays.iterator();i.hasNext(); )
        {
            if(lautoscale) theArrayGraphPanel.addLData((DataArray) i.next());
            else theArrayGraphPanel.addLData((DataArray) i.next(), lymin, lymax);
        }
        for(Iterator i=rDataArrays.iterator();i.hasNext(); )
        {
            if(rautoscale) theArrayGraphPanel.addRData((DataArray) i.next());
            else theArrayGraphPanel.addRData((DataArray) i.next(), rymin, rymax);
        }
        theArrayGraphPanel.showData();
    }
    
    private int maxLength()
    {
        int max=0;
        for(Iterator i=theLData.iterator();i.hasNext(); )
        {
            double[] data=(double[]) i.next();
            if(data.length>max)max=data.length;
        }
        for(Iterator i=theRData.iterator();i.hasNext(); )
        {
            double[] data=(double[]) i.next();
            if(data.length>max)max=data.length;
        }
        return max;
    }
    
    /** ArrayDisplayPanels like to be little squares, if asked.
     * @return a little square hint
     */
    public java.awt.Dimension getPreferredSize()
    {
        return new java.awt.Dimension(200,200);
    }
    
    private static class Labeller implements ArrayLabelGenerator
    {
        /** label the graph with its indices.
         * @param i array index
         * @return text form
         */
        public String getLabel(int i){
            return new Integer(i).toString();
        }
        /** Get the largest expected label.
         * @return assuming we never get an array larger than 100000 points.
         */
        public String getMaximalLabel(){
            return new Integer(99999).toString();
        }
    }
    
    private static final boolean DATACLONING=false;
    
    private static final double[] dataclone(double[] d)
    {
        if(DATACLONING) return (double[]) d.clone();
        else return d;
    }
}
