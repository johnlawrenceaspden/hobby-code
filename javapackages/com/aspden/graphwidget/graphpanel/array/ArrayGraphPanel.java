package com.aspden.graphwidget.graphpanel.array;

import com.aspden.graphwidget.rangecalculators.*;
import com.aspden.graphwidget.unitgraph.*;
import com.aspden.graphwidget.unitgraph.objects.axes.*;
import com.aspden.graphwidget.graphpanel.*;

import com.aspden.graphwidget.misc.*;

import java.awt.*;
import java.util.*;

/** This is the important object for display of multiple arrays.
 * Conceptually it is a collection of partial functions (DataArrays) which
 * act as a GUI component
 * Each partial function is assigned to either the right or left axis.
 * Since this object knows all the data to be displayed, it can determine the ranges
 * on all three axes, and both create the axes and inform the partial functions of
 * the complete dimensions of the graph so that they can scale themselves
 * accordingly for display.
 * Alternatively data can be supplied with a specified y-range, to ensure uniform displays across multiple different graphs.
 * It is possible to draw a box on the graph with the mouse, which will cause a range selection event to be fired through
 * the {@link ArrayRangeSelectionListener } interface using the standard procedure.
 */
public class ArrayGraphPanel extends GraphPanel
{
    private Vector theData;				//DataArrays to be displayed
    private Vector leftAxis;			        //Whether the above are on the right or left
    private IntegerRangeCalculator xRange;              //Range calculation objects
    private DoubleRangeCalculator yRRange,yLRange;
    private ArrayLabelGenerator theArrayLabelGenerator; //How to label the x-axis
    private Vector rangeListeners;                      //Array Range Selection Listeners

    /** Sole Constructor. An empty graph with no data.
     * Add data, range selection listeners, legends and labels and then call {@link #showData }
     */
    public ArrayGraphPanel()
    {
        super();
        clearData();
        rangeListeners = new Vector();
    }

    /**
     * reset the graph leaving listeners and labels attached
     */
    public void clearData()
    {
        theData=new Vector();
        leftAxis=new Vector();
        xRange   = new IntegerRangeCalculator();
        yLRange  = new DoubleRangeCalculator();
        yRRange  = new DoubleRangeCalculator();
    }

    /** Tell this object how it should label the points when it displays itself
     * @param a provides array ordinate labels.
     */
    public void setArrayLabelGenerator(ArrayLabelGenerator a)
    {
        theArrayLabelGenerator=a;
    }

    /** Standard Listener model for range selection events.
     * @param a listens for mouse events.
     */
    public void addRangeListener(ArrayRangeSelectionListener a)
    {
        rangeListeners.addElement(a);
    }

    /** Add data to be displayed against the left axis with a preset range.
     * @param da data to be displayed.
     * @param ymin y value which is forced to be included in the left axis regardless of the actual range in the supplied data.
     * @param ymax y value which is forced to be included in the left axis regardless of the actual range in the supplied data.
     */
    public void addLData(DataArray da, double ymin, double ymax)
    {
        theData.addElement(da);
        leftAxis.addElement(new Boolean(true));
        yLRange.include(ymin);
        yLRange.include(ymax);
        da.includeRangeIn(xRange);
    }

    /** Add data to be displayed against the right axis with a preset range.
     * @param da data to be displayed.
     * @param ymin y value which is forced to be included in the right axis regardless of the actual range in the supplied data.
     * @param ymax y value which is forced to be included in the right axis regardless of the actual range in the supplied data.
     */
    public void addRData(DataArray da, double ymin, double ymax)
    {
        theData.addElement(da);
        leftAxis.addElement(new Boolean(false));
        yRRange.include(ymin);
        yRRange.include(ymax);
        da.includeRangeIn(xRange);
    }

    /** Add a new DataArray on the left axis. The left axis will auto-scale to display the entire range of y-values represented by these data.
     * @param da data to be displayed.
     */
    public void addLData(DataArray da)
    {
        theData.addElement(da);
        leftAxis.addElement(new Boolean(true));
        da.includeValuesIn(yLRange);
        da.includeRangeIn(xRange);
    }

    /** Add a new DataArray on the right axis. The right axis will auto-scale to display the entire range of y-values represented by these data.
     * @param da data to be displayed.
     */
    public void addRData(DataArray da)
    {
        theData.addElement(da);
        leftAxis.addElement(new Boolean(false));
        da.includeValuesIn(yRRange);
        da.includeRangeIn(xRange);
    }

    /**
     * get the coordinate of i in [start, finish] rescaled to [0,1]
     */
    private double xCoord(int i, int start, int finish)
    {
        if (start==finish) return 0.5;
        else return ((double)i-start)/(finish-start);
    }

    /**
     * Create an x-axis for the graph given the starting and finishing array indexes.
     * For less that five points this should label all the points, for more it should
     * be an axis which labels itself evenly according to the size of the window.
     */
    private UnitGraphAxis xAxis(int start, int finish, boolean gridlines)
    {
        if((finish-start)<5)
        {
            UnitGraphConstantAxis a=new UnitGraphConstantAxis(new Compass(Compass.S), gridlines, true);
            for(int i=start; i<=finish; i++)
            {
                double x = xCoord(i, start, finish);
                a.addTick(x,1,getXAxisLabel(x));
            }
            return a;
        }
        else
        {
            UnitGraphAxisLabelGenerator b=new UnitGraphAxisLabelGenerator()
            {
                public String getLabel(double x)
                {
                    return getXAxisLabel(x);
                }
                public String getMaximalLabel()
                {
                    return theArrayLabelGenerator.getMaximalLabel();
                }
            };
            UnitGraphEvenLabelAxis a = new UnitGraphEvenLabelAxis(new Compass(Compass.S), gridlines, b);
            return a;
        }
    }


    /** Called from the superclass to place data and axes.
     */
    protected void showSubclassData()
    {
        //add the axes, provided that there are ranges for them to represent.
        //note that empty ranges cause Exceptions to be thrown which result in the related axis not being added.
        try{
            this.addElement(xAxis(xRange.getMin(), xRange.getMax(), lGrid() || rGrid()));
        } catch(EmptyRangeException e){
            //do not display axis.
        }

        try{
            UnitGraphAxis leftAxis=
            new UnitGraphFloatingPointAxis(new Compass(Compass.W), lGrid(), yLRange.getMin(), yLRange.getMax());
            this.addElement(leftAxis);
        } catch(EmptyRangeException e){
            //do not display axis.
        }

        try{
            UnitGraphAxis rightAxis=
            new UnitGraphFloatingPointAxis(new Compass(Compass.E), rGrid(), yRRange.getMin(), yRRange.getMax());
            this.addElement(rightAxis);
        } catch(EmptyRangeException e){
            //do not display axis.
        }

        /*Tell each DataArray the total range of the graph and attach the UnitGraphObject it represents itself as to the underlying UnitGraph*/

        Enumeration de=theData.elements();
        Enumeration ae=leftAxis.elements();
        while( de.hasMoreElements())
        {
            DataArray a = (DataArray) de.nextElement();
            boolean   b = ((Boolean) ae.nextElement()).booleanValue();
            if(b)
            {
                try{
                    this.addElement(a.getUnitGraphPoints(xRange.getMin(), xRange.getMax(), yLRange.getMin(), yLRange.getMax()));
            } catch(EmptyRangeException e){}	//the only way this can happen (j'espere) is if all the data is NaN. If so we may as well not display it
            }
            else
            {
                try{
                    this.addElement(a.getUnitGraphPoints(xRange.getMin(), xRange.getMax(), yRRange.getMin(), yRRange.getMax()));
            } catch(EmptyRangeException e){}
            }
        }
    }

    /** Called from the superclass to obtain mouse cursor labels.
     * Creates a label like "45<-custom->8.3" using the array ordinate labeller
     * together with the functions used to label the axes.
     * @param x x coord of mouse cursor in [0,1]
     * @param y y coord of mouse cursor in [0,1]
     * @return an appropriate label.
     */
    public String getMouseLabel(double x,double y)
    {

        String label="";

        try{
            double ymin=yLRange.getMin();
            double ymax=yLRange.getMax();
            double yc=y*(ymax-ymin)+ymin;

            label=label + new CoordinateFormatter(ymax-ymin).getCoordText(yc) + " <- " ;

    } catch(EmptyRangeException e){}

        try{
            int xmin=xRange.getMin();
            int xmax=xRange.getMax();
            int xc=(int)Math.round(x*(xmax-xmin)+xmin);
            if(theArrayLabelGenerator!=null)
            {
                label = label + theArrayLabelGenerator.getLabel(xc);
            }
    } catch(EmptyRangeException e){}

        try{
            double ymin=yRRange.getMin();
            double ymax=yRRange.getMax();
            double yc=y*(ymax-ymin)+ymin;

            label=label + " -> " + new CoordinateFormatter(ymax-ymin).getCoordText(yc)  ;

    } catch(EmptyRangeException e){}

        return label;
    }

    /**
     * Given a coordinate in [0,1] get the associated x axis label
     */
    private String getXAxisLabel(double x)
    {
        int xmin, xmax;
        try{
            xmin=xRange.getMin();
            xmax=xRange.getMax();
        }
        catch(EmptyRangeException e)
        {
            //No data so forget it.
            return null;
        }

        int xc=(int)Math.round(x*(xmax-xmin)+xmin);

        String label="";
        if(theArrayLabelGenerator!=null)
        {
            label = theArrayLabelGenerator.getLabel(xc);
        }
        return label;
    }



    /** Called by the superclass when a range selection occurs on the graph.
     * We're not interested in the selected y range, so we convert the x coordinates to
     * array indexes and pass the message upwards
     * @param x1 rectangle coord in [0,1]
     * @param y1 rectangle coord in [0,1]
     * @param x2 rectangle coord in [0,1]
     * @param y2 rectangle coord in [0,1]
     */
    public void rangeSelected(double x1, double y1, double x2, double y2)
    {
        try{
            int xmin=xRange.getMin();
            int xmax=xRange.getMax();
            int x1c=(int)Math.round(x1*(xmax-xmin)+xmin);
            int x2c=(int)Math.round(x2*(xmax-xmin)+xmin);

            Enumeration e=rangeListeners.elements();
            while(e.hasMoreElements())
            {
                ((ArrayRangeSelectionListener)e.nextElement()).rangeSelected(x1c,x2c);
            }
    } catch(EmptyRangeException e){}
    }

    /** Called by the superclass in response to mouse event.
     * @param x click coord in [0,1]
     * @param y click coord in [0,1]
     */
    public void rightButtonReleased(double x, double y)
    {
        Enumeration e=rangeListeners.elements();
        while(e.hasMoreElements())
        {
            ((ArrayRangeSelectionListener)e.nextElement()).rightButtonReleased();
        }
    }

}












