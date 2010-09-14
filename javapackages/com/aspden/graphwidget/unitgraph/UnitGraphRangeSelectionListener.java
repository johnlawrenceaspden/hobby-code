package com.aspden.graphwidget.unitgraph;

/** To be implemented by classes which wish to listen to mouse events from {@link UnitGraphWithMouseSelect} objects.
 */
public interface UnitGraphRangeSelectionListener
{
    /**Called when a rectangle is selected. Corner coordinates in [0,1]x[0,1]*/
    void rangeSelected(double x1, double y1, double x2, double y2);
    void rightButtonReleased(double x, double y);
}
