package com.aspden.graphwidget.linestyles;

import java.awt.*;

/** A style of line where points are large and one colour, and the lines between them are fine and another colour.
 * Only valid points are drawn, and lines are only drawn between two valid points.
 */
public class BobblyLineStyle extends LineStyle
{
	private Color lineColour, pointColour;
	
        /** Creates a new BobblyLineStyle.
         * @param lineColour Colour for lines between points.
         * @param pointColour Colour for the points themselves.
         */
	public BobblyLineStyle(Color lineColour, Color pointColour)
	{
		this.lineColour=lineColour;
		this.pointColour=pointColour;
	}
	
        /** Creates a new BobblyLineStyle where lines and points are the same colour.
         * @param c The colour of the lines and points.
         */
	public BobblyLineStyle(Color c)
	{
		this(c,c);
	}
    	
	public void drawLine( Graphics g, int[] x, int[]y, boolean[] valid)
	{
		Color old=g.getColor();
		g.setColor(lineColour);
		for(int i=1; i<x.length; i++)
		{
		    if(valid[i-1] && valid[i]) g.drawLine(x[i-1],y[i-1],x[i],y[i]);
		}
		g.setColor(pointColour);
		for(int i=0; i<x.length; i++)
		{
		    if(valid[i]) g.fillOval(x[i]-2,y[i]-2,4,4);
		}
		g.setColor(old);
	}
	
}
