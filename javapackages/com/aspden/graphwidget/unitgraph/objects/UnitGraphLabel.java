package com.aspden.graphwidget.unitgraph.objects;

import com.aspden.graphwidget.unitgraph.*;
import com.aspden.graphwidget.misc.*;
import com.aspden.graphwidget.misc.boxobjects.*;

import java.awt.*;

/** A label to live in a corner of a {@link UnitGraph}
 */
public class UnitGraphLabel implements UnitGraphObject
{
	private TextBox theTextBox;
	private Compass theCompass;
	private static final double theInset=0.9;
	
	
        /** Create a new empty label.
         * @param compass Which corner, side or centre to live in.
         */
	public UnitGraphLabel(Compass compass)
	{
		theTextBox=new TextBox();
		theCompass=compass;
	}
	
        /** Create a new label containing a line of text.
         * @param compass The corner to live in.
         * @param s The line of text.
         */
	public UnitGraphLabel(Compass compass, String s)
	{
		this(compass);
		addString(s);
	}
	
        /** Add another line of text.
         * @param s The new line of text.
         */
	public void addString(String s)
	{
		theTextBox.addString(s);
	}

	public void draw(Graphics g, int w, int h)
	{
                //points where labels are to be anchored are just in from the edge of the graph. Constant theInset controls how much in relative to the size of the graph.
		int x=(int)(w*(0.5+theCompass.getWE()*(theInset)/2));
		int y=(int)(h*(0.5-theCompass.getSN()*(theInset)/2));
		Compass direction=theCompass.getOpposite();
		
		(new BoxObjectWrapper(theTextBox)).directedDraw(g,x,y,direction);
	}
}
