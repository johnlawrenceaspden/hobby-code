package com.aspden.graphwidget.graphpanel.array;

/**
 * Interface expressing the ability to provide
 * a String for each element of an array
 */
public interface ArrayLabelGenerator
{
  /** Given an array index return the associated string.
   */
  String getLabel(int i);
  /** Get the largest possible label which can be returned by getLabel.
   */
  String getMaximalLabel();
}
