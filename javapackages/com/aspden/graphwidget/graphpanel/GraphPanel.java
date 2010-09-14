package com.aspden.graphwidget.graphpanel;

import com.aspden.graphwidget.unitgraph.*;
import com.aspden.graphwidget.linestyles.*;
import com.aspden.graphwidget.misc.*;
import com.aspden.graphwidget.unitgraph.objects.*;


import java.awt.*;
import java.util.*;

/**
 * A base class holding common code for more specialized Graph Panels.
 * Holds a UnitGraph and remembers legends and labels and
 * which grids are on and off.
 * It has placeholder functions to provide mouse labels and 
 * respond to selection events, which subclasses must override 
 */
public abstract class GraphPanel extends Panel implements UnitGraphRangeSelectionListener, UnitGraphMouseLabelGenerator
{
	private UnitGraphWithMouseSelect theGraph;
	private Vector theLabelArray;
	private UnitGraphLegendBox theLegendBox;
	private boolean lGridIsShowing, rGridIsShowing;
	
        /** Creates a new GraphPanel with an empty legend box in the SouthWest and gridlines turned off.
         */
	public GraphPanel()
	{
		theLabelArray=new Vector();
		theLegendBox=new UnitGraphLegendBox(new Compass(Compass.SW));
		theGraph=new UnitGraphWithMouseSelect();
		theGraph.addRangeSelectionListener(this);
		theGraph.setMouseLabelGenerator(this);
		lGridIsShowing=false;
		rGridIsShowing=false;
		this.setLayout(new CardLayout());
		this.add(theGraph,"");
	}
        
        /** Subclasses should use this to add {@link UnitGraphObject}s to the underlying graph.
         * @param o The object to be added to the graph.
         */
        protected void addElement(UnitGraphObject o)
        {
            theGraph.addElement(o);
        }
	
        /** Add a new label to the graph.
         * @param a the new label.
         */
	public void addUnitGraphLabel(UnitGraphLabel a)
	{
		theLabelArray.addElement(a);	
	}
	
        /** Add a new legend/linestyle pair to the graph's legend box.
         * @param l The {@link LineStyle} to be labelled.
         * @param s The name of the {@link LineStyle} as it should appear in the legend box.
         */
	public void addLegend(LineStyle l, String s)
	{
		theLegendBox.addLegend(l,s);
	}
	
        /** Kill off the legend box and replace it with a new empty one.
         */
	public void clearLegends()
	{
		theLegendBox=new UnitGraphLegendBox(new Compass(Compass.SW));
	}

	/**
	 * Subclasses must override this function in order to add elements 
	 * to the graph once the labels and legends have been placed
	 */
	protected abstract void showSubclassData();
	
        /** Redisplay the graph. Clear it, then add labels and the legend box, then call {@link #showSubclassData } to let the derived class add its elements.
         */
	public void showData()
	{
		theGraph.clear();
		for(Enumeration e=theLabelArray.elements(); e.hasMoreElements();)
		{
			theGraph.addElement((UnitGraphLabel)e.nextElement());
		}
		if (theLegendBox!=null) theGraph.addElement(theLegendBox);
		showSubclassData();
		theGraph.repaint();
	}
	
	/** Switch on/off the gridlines associated with the data on the right axis
         * @param on true if the grid should be shown, false if not.
         */
	public void setRGrid(boolean on)
	{
		rGridIsShowing=on;
	}
	/**
	 * Switch on/off the gridlines associated with the data on the left axis
         * @param on true if the grid should be shown, false if not.
	 */
	public void setLGrid(boolean on)
	{
		lGridIsShowing=on;
	}
        /** Allows subclasses to ask whether the left axis grid should be on or off.
         * @return true if it should be on, false if not.
         */
	protected boolean lGrid()
	{
		return lGridIsShowing;
	}
        /** Allows subclasses to ask whether the right axis grid should be on or off.
         * @return true if it should be on, false if not.
         */
	protected boolean rGrid()
	{
		return rGridIsShowing;
	}
	
	/**
	 *Switch on the ability to draw a box with the mouse which fires a selection event
	 */
	public void enableSelection()
	{
	    theGraph.enableSelection();
	}
	
	/**
	 *Switch off the ability to draw a box with the mouse 
	 */
	public void disableSelection()
	{
	    theGraph.disableSelection();
	}

	/** Listens for range selection events from the underlying {@link UnitGraphWithMouseSelect }.
         * Must be overridden by subclasses to determine what to do with the selection event.
         * @param x1 coordinate of selected rectangle in range [0,1]
         * @param y1 coordinate of selected rectangle in range [0,1]
         * @param x2 coordinate of selected rectangle in range [0,1]
         * @param y2 coordinate of selected rectangle in range [0,1]
         */
	abstract public void rangeSelected(double x1, double y1, double x2, double y2);
        
	/** Listens for right button release events from the underlying {@link UnitGraphWithMouseSelect }.
         * Must be overridden by subclasses to determine what to do with the selection event.
         * @param x coordinate of selected rectangle in range [0,1]
         * @param y coordinate of selected rectangle in range [0,1]
         */
        abstract public void rightButtonReleased(double x, double y);
        
	/** Must be overridden by subclasses to provide labels for the mouse cursor.
         * @param x coordinate of cursor in range [0,1]
         * @param y coordinate of cursor in range [0,1]
         * @return the label which is to be displayed for the mouse cursor.
         */
	abstract public String getMouseLabel(double x, double y);
}
