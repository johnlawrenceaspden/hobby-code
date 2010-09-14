package com.aspden.graphwidget.graphpanel.array;

import com.aspden.graphwidget.linestyles.*;
import com.aspden.graphwidget.misc.*;
import com.aspden.graphwidget.rangecalculators.*;
import com.aspden.graphwidget.unitgraph.*;
import com.aspden.graphwidget.unitgraph.objects.*;



import java.util.*;
import java.awt.*;

/** Represents data to be displayed on an {@link ArrayGraphPanel }
 * A function from a section of the integers to R union NaN.
 * NaN is to represent missing values.
 * It can include its values in Range calculation objects
 * and express itself as a UnitGraphPoints object.
 * It includes a {@link LineStyle} object which tells it how to draw itself.
 */
public class DataArray
{
    private int startIndex;
    private double[] y;
    private LineStyle theLineStyle;
    /** Create a (not necessarily zero based) array to be displayed on an {@link ArrayGraphPanel}.
     * @param startindex Index of first point in array.
     * @param y the data
     * @param l the style which is to be used when the data are displayed.
     */
    public DataArray(int startindex, double y[], LineStyle l)
    {
        this.y = y;
        this.startIndex=startindex;
        this.theLineStyle=l;
    }

    /** Add the function values to a range calculator
     * @param r The range calculator which is to include the y values.
     */
    public void includeValuesIn(DoubleRangeCalculator r)
    {
        for(int i=0; i<y.length; i++)
        {
            r.include(y[i]);
        }
    }

    /** Add the function range (the x coordinates) to a range calculator.
     * @param i The range calculator to which the x coords are to be added.
     */
    public void includeRangeIn(IntegerRangeCalculator i)
    {
        i.include(startIndex);
        i.include(startIndex+y.length-1);
    }

    /** Given a scale for the {@link ArrayGraphPanel} as a whole, express the function as a {@link UnitGraphPoints} object suitable to be added to it.
     * @param xmin min x range of whole graph.
     * @param xmax max x range of whole graph.
     * @param ymin min y range of whole graph.
     * @param ymax max y range of whole graph.
     * @return An appropriately scaled UnitGraphPoints object
     */
    public UnitGraphPoints getUnitGraphPoints(int xmin, int xmax, double ymin, double ymax)
    {
        double xc[]=new double[y.length];
        double yc[]=new double[y.length];
        for(int i=0; i<y.length; i++)
        {
            if(Double.isNaN(y[i]))
            {
                xc[i]=yc[i]=Double.NaN;
            }
            else
            {
                xc[i]=((double)(i+startIndex-xmin))/(xmax-xmin);
                yc[i]=(y[i]-ymin)/(ymax-ymin);
            }
        }
        return new UnitGraphPoints(xc,yc,theLineStyle);
    }

    /** Test function providing a random instance
     * @param range roughly how many points to create.
     * @param style style to display the object.
     * @return a random set of data
     */
    public static DataArray getSample(int range, LineStyle style)
    {
        if(range<1) throw new IllegalArgumentException();

        Random r=new Random();

        int start = r.nextInt(range);
        int len   = r.nextInt(range)+10;

        double x[]=new double[len];
        x[0]=10.0;
        for(int i=1;i<len;i++)
        {
            x[i]=x[i-1]+2*(r.nextDouble()-0.5);//len;
        }
        for(int i=0;i<len;i++)
        {
            if(r.nextInt(7)==0) x[i]=Double.NaN;
        }

        return new DataArray(start,x,style);
    }
}
