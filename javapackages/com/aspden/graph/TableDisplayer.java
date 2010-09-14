/*
 * TableDisplayer.java
 */

package com.aspden.graph;

/** Interface implemented by objects which display a 2D table of components.
 */
public interface TableDisplayer {
    /** Add a component in a certain position in a table.
     * @param c The component to be added to the table.
     * @param row The row in which the component is to be displayed.
     * @param column The column in which the component is to be displayed.
     */
  public void add(java.awt.Component c, int row, int column);
    /** Cause the TableDisplayer to become visible.
     */
  public void show();
    /** Highlight a particular component in the table.
     * @param row The row of the highlighted component.
     * @param column The column of the highlighted component.
     */
  public void highlight(int row, int column);
}