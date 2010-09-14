package com.aspden.graphwidget.unitgraph.objects.axes;

import com.aspden.graphwidget.misc.*;
import com.aspden.graphwidget.misc.boxobjects.*;
import com.aspden.graphwidget.unitgraph.objects.*;

import java.awt.*;
import java.util.*;



/** An axis at one edge of the unit square.
 * Orientation values can be Compass.N, E, S, W.
 * An East Axis is on the right of the Unit Square and is thus vertical.
 */
public abstract class UnitGraphAxis implements UnitGraphObject
{
    private Compass orientation;
    private boolean gridlines;
    private boolean leftlabels;

    /** An axis can be on the North, South, East or West of the graph,
     * can have associated gridlines or not, and can have its labels
     * displaced slightly to the left or placed exactly on the ticks
     * @param orientation Side of the graph to place the axis.
     * @param gridlines Do the axis ticks have associated gridlines?
     * @param leftlabels Should the labels be positioned over the ticks or displaced slightly?
     */
    public UnitGraphAxis(Compass orientation, boolean gridlines, boolean leftlabels)
    {
        if(!orientation.isCardinal())
        {
            throw new IllegalArgumentException();
        }
        this.orientation=orientation;
        this.gridlines=gridlines;
        this.leftlabels=leftlabels;
    }

    /** An axis can be on the North, South, East or West of the graph, and can have associated gridlines or not.
     * @param orientation Side of the graph to place the axis.
     * @param gridlines Do the axis ticks have associated gridlines?
     */
    public UnitGraphAxis(Compass orientation, boolean gridlines)
    {
        this(orientation, gridlines, false);
    }

    private boolean isVertical()
    {
        return orientation.isEW();
    }
    
    /** To be called by subclasses when determining how many labels they should generate.
     * @param g The graphics context to be drawn into.
     * @param width The width of the graph in pixels.
     * @param height The height of the graph in pixels.
     * @param longestLabel The longest label which may be placed.
     * @return The number of labels which can fit happily on the axis.
     */
    protected double axisLengthInLabels(Graphics g, int width, int height, String longestLabel)
    {
        FontMetrics fm=g.getFontMetrics();
        if(isVertical())
        {
            return (double)height/(fm.getHeight()*2);
        }
        else
        {
            return (double)width/(fm.stringWidth(longestLabel+"   "));
        }
    }

    /**
     * given the x&y coordinates of the base of the tick, top of the tick, and top of the gridline
     * p0 p1                                     p2
     * ===---------------------------------------
     * and the label and which direction it is to be drawn relative to the top of the tick,
     * draw the
     */
    private void drawTick(Graphics g, int x0, int y0, int x1, int y1, int x2, int y2, String label, Compass offset, boolean drawingGrid)
    {
        if(gridlines && drawingGrid)
        {
            Color c=g.getColor();
            g.setColor(Color.lightGray);
            g.drawLine(x0,y0,x2,y2);
            g.setColor(c);
        }
        if(!drawingGrid)
        {
            g.drawLine(x0,y0,x1,y1);
            (new BoxObjectWrapper(new TextBox(label))).directedDraw(g,x1,y1,offset);
        }
    }

    /** Subclasses should call this function from the doTicks function which they must implement in order to draw labels, ticks and gridlines.
     * A sample call might draw a tick labelled "My Tick" at 0.7 of the way along the axis with priority 1, meaning that it is the biggest sort of tick.
     * @param g The graphics context to be drawn into.
     * @param width The width of the graph in pixels.
     * @param height The width of the graph in pixels.
     * @param x The position of the tick in the range [0,1].
     * @param priority The priority (size) of the tick.
     * @param label The label for this axis tick.
     * @param drawingGrid Whether to draw the grid or the tick. To enable gridlines to be drawn under everything else.
     */
    protected void drawTick(Graphics g, int width, int height, double x, int priority, String label, boolean drawingGrid)
    {
        int x0,y0,x1,y1,x2,y2;
        Compass offset;
        int ticklen=2/priority+1;

        if(orientation.isNS())
        {
            x0=x1=x2=(int)(width*x);
            if(orientation.isN())
            {
                y0=0;
                y1=ticklen;
                y2=height;
                if(leftlabels) offset=new Compass(Compass.SW);
                else offset=new Compass(Compass.S);
            }
            else
            {
                y0=height;
                y1=height-ticklen;
                y2=0;
                if(leftlabels) offset=new Compass(Compass.NW);
                else offset=new Compass(Compass.N);
            }
        }
        else
        {
            y0=y1=y2=(int)(height*(1-x));

            {	//don't want EW axes interfering with NS axes, so there's a
                //band in which we shouldn't put ticks.
                int band = 3*((g.getFontMetrics()).getHeight())/2;
                if(y0<band || y0>(height-band)) return;
            }
            if(orientation.isW())
            {
                x0=0;
                x1=ticklen;
                x2=width;
                offset=new Compass(Compass.E);
            }
            else
            {
                x0=width;
                x1=width-ticklen;
                x2=0;
                offset=new Compass(Compass.W);
            }
        }
        drawTick(g,x0,y0,x1,y1,x2,y2, label, offset, drawingGrid);
    }

    /** Subclasses must override this with a function that calls the protected function drawTick(g, x, priority, label) for each x at which a tick is to be placed.
     * @param g The graphics context to be drawn into.
     * @param width The width of the graph in pixels.
     * @param height The height of the graph in pixels.
     * @param drawingGrid Whether to draw the grid or the tick. To enable gridlines to be drawn under everything else.
     */
    protected abstract void doTicks(Graphics g, int width, int height, boolean drawingGrid);

    /** Draws the gridlines without the ticks.
     * This is so that the containing graph can make sure that one axis'
     * gridlines don't cover another one's ticks.
     * @param g The graphics context to be drawn into.
     * @param w The width of the graph in pixels.
     * @param h The height of the graph in pixels.
     */
    public final void drawGrid(Graphics g, int w, int h)
    {
        this.doTicks(g, w, h, true);
    }

    /** Draws the ticks without the grid lines.
     * This is so that the containing graph can make sure that one axis'
     * gridlines don't cover another one's ticks.
     * @param g The graphics context to be drawn into.
     * @param w The width of the graph in pixels.
     * @param h The height of the graph in pixels.
     */
    public final void draw(Graphics g, int w, int h)
    {
        this.doTicks(g, w, h, false);
    }

}
