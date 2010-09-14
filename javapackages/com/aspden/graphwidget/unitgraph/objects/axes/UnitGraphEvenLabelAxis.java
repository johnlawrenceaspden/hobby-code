package com.aspden.graphwidget.unitgraph.objects.axes;

import java.awt.*;

import com.aspden.graphwidget.misc.*;
import com.aspden.graphwidget.unitgraph.*;



/** An axis which fits in as many evenly spaced ticks/labels as it can, drawing these from an external source.
 */
public class UnitGraphEvenLabelAxis extends UnitGraphAxisWithLabelGenerator
{

    /** Create an externally labelled evenly spaced axis.
     * @param orientation Which edge of the {@link UnitGraph} the axis is to live on.
     * @param gridlines Is the axis to have associated gridlines?
     * @param labeller the external source of labels.
     */
    public UnitGraphEvenLabelAxis(Compass orientation, boolean gridlines, UnitGraphAxisLabelGenerator labeller)
    {
        super(orientation, gridlines, labeller, true);
    }

    protected void doTicks(Graphics g, int width, int height, boolean drawingGrid)
    {
        for(double x=1.0; x>=0.0; x-=(1.0/axisLengthInLabels(g, width, height, getMaximalLabel())))
        {
            String label;
            drawTick(g,width, height, x, 1, getLabel(x), drawingGrid);
        }
    }
}
