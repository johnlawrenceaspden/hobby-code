package com.aspden.graphwidget.unitgraph.objects.axes;

import java.util.*;
import java.awt.*;

import com.aspden.graphwidget.misc.*;
import com.aspden.graphwidget.unitgraph.*;

/** An axis for a {@link UnitGraph} which has a fixed set of labels, which are always placed regardless of space.
 */
public class UnitGraphConstantAxis extends UnitGraphAxis
{
	private Vector coords = new Vector();
        private Vector priorities = new Vector();
        private Vector labels = new Vector();
	
        /** Create an empty UnitGraphConstantAxis.
         * @param orientation Which edge is the axis to live on?
         * @param gridlines Should the axis have gridlines?
         * @param leftlabels Should the labels be placed directly over the ticks or to the left?
         */
	public UnitGraphConstantAxis(Compass orientation, boolean gridlines, boolean leftlabels)
	{
		super(orientation, gridlines, leftlabels);
	}
        
        /** Create an empty UnitGraphConstantAxis.
         * @param orientation Which edge is the axis to live on?
         * @param gridlines Should the axis have gridlines?
         */
	public UnitGraphConstantAxis(Compass orientation, boolean gridlines)
	{
		this(orientation, gridlines, false);
	}
	
        /** Place a new label on the axis.
         * @param x point in [0,1] to be labelled.
         * @param priority size of tick.
         * @param label Label for tick.
         */
	public void addTick(double x, int priority, String label)
	{
		coords.addElement(new Double(x));	
		priorities.addElement(new Integer(priority));
		labels.addElement(label);
	}
	
	protected void doTicks(Graphics g, int width, int height, boolean drawingGrid)
	{
		
		Enumeration ce=coords.elements();
		Enumeration pe=priorities.elements();
		Enumeration le=labels.elements();
		
		while(ce.hasMoreElements())
		{
			double x = ((Double) ce.nextElement()).doubleValue();
			int p = ((Integer) pe.nextElement()).intValue();
			String l = (String) le.nextElement();
			drawTick(g, width, height,  x, p, l, drawingGrid);
		}
	}
	

}
