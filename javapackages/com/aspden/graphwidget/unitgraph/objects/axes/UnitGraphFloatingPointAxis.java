package com.aspden.graphwidget.unitgraph.objects.axes;

import com.aspden.graphwidget.misc.*;
import com.aspden.graphwidget.unitgraph.*;

import java.awt.*;
import java.util.*;
import java.text.*;


/**
 * An axis which will represent a certain floating point range and label itself according to the size of the graph on screen.
 */
public class UnitGraphFloatingPointAxis extends UnitGraphAxis
{
    private double a,b;

    /** Create a UnitGraphFloatingPointAxis
     * @param orientation Which edge of the {@link UnitGraph} the axis is to live on.
     * @param gridlines Is the axis to have associated gridlines?
     * @param a left hand end of floating point range.
     * @param b right hand end of floating point range.
     */
     public UnitGraphFloatingPointAxis( Compass orientation, boolean gridlines, double a, double b)
    {
        super(orientation, gridlines);
        this.a=a;
        this.b=b;
    }

    protected void doTicks(Graphics g, int width, int height, boolean drawingGrid)
    {
        CoordinateFormatter formatter=new CoordinateFormatter(b-a);

        //Given the total length in pixels and the size of the largest possible label,
        //get a load of round numbers which can be used as axis ticks without overlapping.
        Enumeration e;
        e=cardinalPositions(axisLengthInLabels(g, width, height, formatter.getMaximalCoordText())).elements();

        //Then use the superclass function to plot them all.
        while(e.hasMoreElements())
        {
            double x=((Double)e.nextElement()).doubleValue();
            drawTick(g, width, height, (x-a)/(b-a),1,formatter.getCoordText(x), drawingGrid);
        }
    }



    /**
     * Gets a selection of round numbers in a range.
     */
    private Vector cardinalPositions(double axisLengthInLabels )
    {
        //gap in pixels to a nice gap in coordinates
        double dx=1.0/axisLengthInLabels;
        dx *= (b-a);
        if(dx<0) dx = -dx;
        dx = roundUp(dx);

        //find the least round value in the range [a,b]
        double guess=a/dx;
        guess=Math.floor(guess);
        double min=guess*dx;
        while (min > a) min -=dx;
        while (min < a) min +=dx;

        //Enumerate the round values in the range
        Vector v=new Vector();
        if(b>a)	for(double x=min; x<=b; x+=dx) v.addElement(new Double(x));
        else for(double x=min; x>=b; x-=dx) v.addElement(new Double(x));

        return v;
    }

    /**
     * Rounds up to numbers like 100, 50, 25, 12.5, 10, 5, 2.5, 1.25, 1, 0.5, 0.25
     */
    private double roundUp(double x)
    {
        //find the power of 10 just above x
        double r=1.0;
        while (r>x) r/=10;
        while (r<x) r *=10;

        //keep halving it, but don't let it go below x
        while (r/2>x) r/=2;
        return r;
    }
}
