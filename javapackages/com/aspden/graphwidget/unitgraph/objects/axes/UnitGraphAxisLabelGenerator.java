package com.aspden.graphwidget.unitgraph.objects.axes;

/** To be implemented by objects wishing to provide labels for unit graph axes.
 */
public interface UnitGraphAxisLabelGenerator
{
    /** Provide a label for the point x in the range [0,1].
     * @param x the point in [0,1] to be labelled.
     * @return the appropriate label.
     */
    public String getLabel(double x);
    /** Get the largest label which can be provided by this labeller.
     * Allows the axis to calculate how many labels can fit on the graph.
     * @return the longest possible label.
     */
    public String getMaximalLabel();
}
