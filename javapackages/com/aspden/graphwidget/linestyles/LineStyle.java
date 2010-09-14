package com.aspden.graphwidget.linestyles;

import java.awt.*;

/**
 * LineStyle derivatives draw styled lines on graphics contexts given sets of points.
 */
abstract public class LineStyle
{
    /** Causes a line to be drawn given a set of points, some of which may be invalid.
     * Consider <I>e.g.</I> a time series with some missing values.
     * It is up to the line style to decide what to do about missing values.
     * <I>E.g.</I> it could decide to break the line at that point, or to connect the points on either side with a dotted line.
     *
     *
     * @param g The graphics context into which the line is to be drawn.
     * @param x The x coordinates of the points to be plotted.
     * @param y The y coordinates of the points to be plotted.
     * @param valid The array indicating which points are valid.
     */
    abstract public void drawLine( Graphics g, int[] x, int[] y, boolean[] valid);

    /** Simpler version if all the points are valid.
     * @param g The graphics context into which the line is to be drawn.
     * @param x The x coordinates of the points to be plotted.
     * @param y The y coordinates of the points to be plotted.
     */
    public void drawLine( Graphics g, int[]x, int[]y)
    {
        boolean[] valid=new boolean[x.length];
        for(int i=0; i<x.length; i++) valid[i]=true;
        drawLine(g, x, y, valid);
    }

    /** Draw a line in the style between two points.
     * @param g Graphics context.
     * @param x1 x-coord of point 1
     * @param y1 y-coord of point 1
     * @param x2 x-coord of point 2
     * @param y2 y-coord of point 2
     */
    public void drawLine(Graphics g, int x1, int y1, int x2, int y2)
    {
        int[] x=new int[2];
        int[] y=new int[2];
        boolean[] valid=new boolean[2];

        x[0]=x1; x[1]=x2;
        y[0]=y1; y[1]=y2;
        valid[0]=true; valid[1]=true;

        drawLine(g, x, y, valid);
    }
}
