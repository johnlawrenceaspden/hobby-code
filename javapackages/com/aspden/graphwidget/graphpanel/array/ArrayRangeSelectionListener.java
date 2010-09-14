package com.aspden.graphwidget.graphpanel.array;

/** The interface to be implemented by objects which wish to listen to array index range selection events.
 */
public interface ArrayRangeSelectionListener {
  /** An array range has been selected.
   * @param a left hand end of range
   * @param b right hand of range
   */
  public void rangeSelected(int a,int b);

/** A right click has occurred on the graph.
 */
  public void rightButtonReleased();


}
