/*
 * Statistic.java
 */
 
package com.aspden.tespar.statistics;

import java.util.*;

/** A Statistic is a representation derived from a TESPAR symbol stream, such as an A Matrix or S Matrix,
 * or possibly a more exotic form such as Tim Phipps' three part S Matrix derived from a TESPARgram.
 */
public abstract class Statistic extends Object {
  /**Takes a list of statistics and returns their average*/
  public abstract Statistic asAverage(List list);
  /**Return a correlation score between 'this' and s. Typically something like the inner product angle or euclidean distance.
  Higher should be worse!!*/
  public abstract double correlate(Statistic s);
  /**Create a visual component to represent the statistic: a graph, histogram, colour matrix etc.*/
  public abstract java.awt.Component visualise();
  /**return full name of class i.e. "duration weighted A-matrix"*/
  public abstract String getStatisticType();
}