package com.aspden.graphwidget.unitgraph.objects.axes;


import com.aspden.graphwidget.misc.*;
import com.aspden.graphwidget.unitgraph.*;

/** A base class for {@link UnitGraphAxis} objects which can take their labels from an external source.
 */
abstract public class UnitGraphAxisWithLabelGenerator extends UnitGraphAxis
{
    private UnitGraphAxisLabelGenerator theAxisLabelGenerator;

    /** Create an externally labelled {@link UnitGraphAxis }
     * @param orientation Which edge of the {@link UnitGraph} the axis is to live on.
     * @param gridlines Is the axis to have associated gridlines?
     * @param labeller the external source of labels.
     * @param leftlabels Are the labels to be placed over the ticks or slightly to the left?
     */
    public UnitGraphAxisWithLabelGenerator(Compass orientation, boolean gridlines, UnitGraphAxisLabelGenerator labeller, boolean leftlabels)
    {
        super(orientation, gridlines, leftlabels);
        theAxisLabelGenerator=labeller;
    }

    /** Subclasses call this to get their labels.
     * @param x point in [0,1] to be labelled.
     * @return the label.
     */
    protected String getLabel(double x)
    {
        if(theAxisLabelGenerator!=null)
        {
            return theAxisLabelGenerator.getLabel(x);
        }
        else return null;
    }

    /** Subclasses call this to find out the longest label they can expect when working out how many labels can be placed on an axis.
     * @return The longest possible label.
     */
    protected String getMaximalLabel()
    {
        if(theAxisLabelGenerator!=null)
        {
            return theAxisLabelGenerator.getMaximalLabel();
        }
        else return null;
    }
}
