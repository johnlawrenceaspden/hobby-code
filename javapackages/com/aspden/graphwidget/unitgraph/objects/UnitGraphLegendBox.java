package com.aspden.graphwidget.unitgraph.objects;

import com.aspden.graphwidget.misc.*;
import com.aspden.graphwidget.linestyles.*;
import com.aspden.graphwidget.unitgraph.*;


import java.awt.*;
import com.aspden.graphwidget.misc.boxobjects.*;

/** A Legend Box (collection of {@link LineStyle}s and labels) for a {@link UnitGraph}.
 */
public class UnitGraphLegendBox implements UnitGraphObject
{
	private LegendBox theLegendBox;
	private Compass theCompass;
	private static final double theInset=0.9;
	
	
        /** Create a new Legend Box which will live in a corner of the {@link UnitGraph}.
         * @param compass The corner of the graph to live in.
         */
	public UnitGraphLegendBox(Compass compass)
	{
		theLegendBox=new LegendBox();
		theCompass=compass;
	}
	
        /** Add a new LineStyle/label pair to the Legend Box.
         * @param l LineStyle to be labelled.
         * @param s The label.
         */
	public void addLegend(LineStyle l, String s)
	{
		theLegendBox.addLegend(l,s);
	}

	public void draw(Graphics g, int w, int h)
	{
		int x=(int)(w*(0.5+theCompass.getWE()*(theInset)/2));
		int y=(int)(h*(0.5-theCompass.getSN()*(theInset)/2));
		Compass direction=theCompass.getOpposite();
		
		(new BoxObjectWrapper(theLegendBox)).directedDraw(g,x,y,direction);
	}
}
