package com.aspden.graphwidget.unitgraph;

/**
 * To provide mouse labels for Unit Graphs.
 */
public interface UnitGraphMouseLabelGenerator
{
    /**given coords in the [0,1]x[0,1] square what is an appropriate label for the mouse pointer?*/
    String getMouseLabel(double x, double y);
}
