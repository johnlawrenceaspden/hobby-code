package com.aspden.graphwidget.unitgraph;

import com.aspden.graphwidget.misc.*;
import com.aspden.graphwidget.unitgraph.*;
import com.aspden.graphwidget.unitgraph.objects.*;
import com.aspden.graphwidget.unitgraph.objects.axes.*;



import java.awt.*;
import java.awt.event.*;
import java.util.*;

/** A component which contains {@link UnitGraphObject}s and displays them.
 * It conceptually has a coordinate system [0,1]x[0,1]
 * with origin in the bottom left corner, but can assume any rectangular shape
 * on the screen.
 */
public class UnitGraph extends BufferedCanvas
{
	Vector theUnitGraphObjects=new Vector();

        /** Add a {@link UnitGraphObject} to the graph.
         * @param p the {@link UnitGraphObject}
         */
	public void addElement(UnitGraphObject p)
	{
		theUnitGraphObjects.addElement(p);
	}
	
        /** Remove all elements from the graph.
         */
	public void clear()
	{
		theUnitGraphObjects=new Vector();
	}
	
	/** We display all the objects which have been added in order,
         * with the twist that Axis objects are given the chance to draw
         * their gridlines first.
         * @param g The graphics context of the double buffer inherited from {@link BufferedCanvas}
         */
	protected void paintImage(Graphics g)
	{
		for(Enumeration e=theUnitGraphObjects.elements();e.hasMoreElements();)
		{
			UnitGraphObject o = (UnitGraphObject) e.nextElement();
			if (o instanceof UnitGraphAxis)
			{
				((UnitGraphAxis)o).drawGrid(g,getSize().width,getSize().height);
			}
		}
		for(Enumeration e=theUnitGraphObjects.elements();e.hasMoreElements();)
		{
			UnitGraphObject o = (UnitGraphObject) e.nextElement();
			o.draw(g,getSize().width,getSize().height);
		}
	}
	
        /** Given a real pixel coordinate convert it to the [0,1] range.
         * @param x the pixel coordinate
         * @return a coordinate in the 0,1 range.
         */
	protected double xCoord(int x)
	{
		return (double)x/getSize().width;
	}
	
        /** Given a real pixel coordinate convert it to the [0,1] range.
         * @param y the pixel coordinate
         * @return a coordinate in the 0,1 range.
         */
	protected double yCoord(int y)
	{
		return 1-((double)y/getSize().height);
	}

}
