package com.aspden.graphwidget.misc.boxobjects;

import java.util.*;
import java.awt.*;

/**
 * A number of strings to be drawn as separate lines in a rectangle
 */
public class TextBox implements BoxObject
{
    private Vector theStrings;
    private int ascent, descent, width, height;

    /** Creates a new empty TextBox.
     */
    public TextBox()
    {
        theStrings=new Vector();
    }

    /** Creates a text box with a single line of text.
     * @param s The single line of text.
     */
    public TextBox(String s)
    {
        this();
        addString(s);
    }

    /** Add another line to a TextBox.
     * @param s The line of text to be added.
     */
    public void addString(String s)
    {
        theStrings.addElement(s);
    }

    public void draw(Graphics g, int x, int y)
    {
        if(theStrings.isEmpty()) return;

        calculateSize(g);

        y+=ascent;

        for(Enumeration e=theStrings.elements();e.hasMoreElements();)
        {
            g.drawString((String)(e.nextElement()),x,y);
            y+=height;
        }
    }

    private void calculateSize(Graphics g)
    {
        FontMetrics fm = g.getFontMetrics();

        width=0;
        ascent=fm.getAscent();
        descent=fm.getDescent()+fm.getHeight()*(theStrings.size()-1);
        height=fm.getHeight();

        for(Enumeration e=theStrings.elements();e.hasMoreElements();)
        {
            int w=fm.stringWidth((String)(e.nextElement()));
            if(width < w) width=w;
        }
    }

    public int getWidth(Graphics g)
    {
        calculateSize(g);
        return width;
    }

    public int getDepth(Graphics g)
    {
        calculateSize(g);
        return ascent+descent;
    }
}

