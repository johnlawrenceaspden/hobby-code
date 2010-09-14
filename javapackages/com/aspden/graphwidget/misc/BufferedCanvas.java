package com.aspden.graphwidget.misc;



import java.awt.*;
import java.awt.event.*;

/**
 * A Canvas derivative implementing double-buffering.
 * Subclasses should override paintImage instead of paint in order to draw.
 */
public abstract class BufferedCanvas extends Canvas
{
    /** paint is overriden so that the double-buffer redrawn and then copied onto the screen
     * @param g Graphics context ( passed in by the system )
     */
    public void paint(Graphics g)
    {
        Image theImage=createImage(getSize().width, getSize().height);
        Graphics s=theImage.getGraphics();
        try{
            paintImage(s);
            g.drawImage(theImage,0,0,this);
        }finally{
            s.dispose();
        }
    }

    /** Overridden to prevent flickering. This stops the superclass update method redrawing the background and then calling paint.
     * @param g Graphics context (passed in by the system).
     */
    public void update(Graphics g)
    {
        paint(g);
    }

    /** Subclasses should override this instead of paint to gain double buffering.
     * @param g Graphics context. Passed in by Buffered Canvas. Draws onto the double buffer.
     */
    protected abstract void paintImage(Graphics g);
}
