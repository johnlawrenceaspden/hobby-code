package com.aspden.graphwidget.linestyles;

import java.awt.*;

/** A line like that produced by a calligraphic pen.
 */
public class ThickLineStyle extends LineStyle
{
    private Color colour;
    private int thickness;

    /** Creates a new ThickLineStyle.
     * @param colour colour of line
     * @param thickness thickness of line in pixels.
     */
    public ThickLineStyle(Color colour, int thickness)
    {
        this.thickness=thickness;
        this.colour=colour;
    }

    public void drawLine( Graphics g, int[] x, int[] y, boolean[] valid)
    {
        Color old=g.getColor();
        g.setColor(colour);
        for(int j=0;j<thickness; j++)
        {
            for(int i=1; i<x.length; i++)
            {
                if(valid[i-1] && valid[i]) g.drawLine(x[i-1],y[i-1]+j,x[i],y[i]+j);
            }
        }
        g.setColor(old);
    }
}
