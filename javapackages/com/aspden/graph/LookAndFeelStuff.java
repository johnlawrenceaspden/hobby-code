/*
 * LookAndFeelStuff.java
 */

package com.aspden.graph;

import javax.swing.*;

/** Exists only to provide the look and feel randomising function.
 * A class about as spectral as it gets.
 */
public class LookAndFeelStuff extends Object {

  /** Choose a look and feel at random
   */
  public static void randomise()
  {
    try{
      UIManager.LookAndFeelInfo[] a= UIManager.getInstalledLookAndFeels()  ;
      UIManager.setLookAndFeel(a[new java.util.Random().nextInt(a.length)].getClassName());

    }catch(Exception e){
      System.out.println(e);
    }
  }
}