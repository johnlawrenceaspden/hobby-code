package com.aspden.graphwidget.misc.boxobjects;

import com.aspden.graphwidget.misc.*;

import java.awt.*;

/**
 * A wrapper which allows BoxObjects 
 * to draw themselves into a graphics context at
 * any compass direction relative to a given point
 */
public class BoxObjectWrapper
{
	private BoxObject theBoxObject;
	
        /** Decorate a BoxObject with the ability to draw itself relative to any corner.
         * @param a The BoxObject to be decorated.
         */
	public BoxObjectWrapper(BoxObject a)
	{
		theBoxObject=a;
	}
	
        /** Cause the decorated BoxObject to draw itself relative to a point.
         * @param g The graphics context to draw into.
         * @param x The x coordinate of the corner.
         * @param y The y coordinate of the corner.
         * @param direction The corner to be drawn at (x,y).
         */
	public void directedDraw(Graphics g, int x, int y, Compass direction)
	{
		int width=theBoxObject.getWidth(g);
		int depth=theBoxObject.getDepth(g);
		
		switch(direction.getWE())
		{
			case 0:
					x-=width/2;
					break;
			case -1:
					x-=width;
					break;
			default:
					break;
		}
		switch(direction.getSN())
		{
			case 1:
					y-=depth;
					break;
			case 0:
					y-=depth/2;
					break;
			default:
					
					break;
		}
		
		theBoxObject.draw(g,x,y);

	}
}
