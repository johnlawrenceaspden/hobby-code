package com.aspden.graphwidget.misc.boxobjects;

import java.awt.*;

/**
 * A BoxObject has a width and depth dependent on the graphics
 * context, and can draw itself into a graphics context,
 * given the point which is to be the top left corner
 */
public interface BoxObject
{
    /** How wide would you be if drawn into this Graphics context?
     * @param g The graphics context which is to determine eg the sizes of fonts.
     * @return the width in this context.
     */
    public int getWidth(Graphics g);
    /** How tall would you be if drawn into this Graphics context?
     * @param g The graphics context which is to determine eg the sizes of fonts.
     * @return the height in this context.
     */
    public int getDepth(Graphics g);
    /** Draw yourself into this graphics context given (x,y) as the top left corner.
     * @see BoxObjectWrapper
     * @param g Graphics context to draw into.
     * @param x x coordinate of top left hand corner.
     * @param y y coordinate of top left hand corner.
     */
    public void draw(Graphics g, int x, int y);
}
