package com.aspden.graph;

import java.awt.*;
import com.aspden.graphwidget.misc.*;

/** Frame to display a single visual component and System.exit() on close.
 * It's purely a convenience class.
 * Various constructors allow one to specify name and size, or use defaults
 */
public class SingleComponentFrame extends CloseableFrame {


  /**This is one of those functions which one has to write because one is not allowed to call a constructor in the second line of another constructor.*/
  private void constructorCommonCode(Component c, int width, int height, boolean pack)
  {
    setLayout(new java.awt.BorderLayout());
    add(c);
    if(pack)pack(); else setSize(width,height);
    show();
  }

  /** Create a window to contain a component specifying name and size.
 * @param c The component to be displayed in the Frame
 * @param title The title of the Frame
 * @param width The width in pixels.
 * @param height The height in pixels.
 */
  public SingleComponentFrame( Component c, String title, int width, int height)
  {
    super(title);
    constructorCommonCode(c,width,height, false);
  }

  /** Create a window to contain a component specifying name and size in fractions of the screen size.
 * @param c The component to be displayed in the Frame
 * @param title The title of the Frame
 * @param width the width as a fraction of the screen width.
 * @param height the height as a fraction of the screen height.
 */
  public SingleComponentFrame(Component c, String title, double width, double height){
    super(title);
    Dimension d=Toolkit.getDefaultToolkit().getScreenSize();
    constructorCommonCode(c,(int)(d.width*width),(int)(d.height*height), false);
  }

  /** Create a window to contain a component with no name and without specifying size.
 * @param c The component to be displayed in the Frame
 */
  public SingleComponentFrame(Component c) {
    super("");
    constructorCommonCode(c,0,0, true);
  }

  /** Create a window to contain a component specifying the name but not the size.
 * @param c The component to be displayed in the Frame
 * @param title The title of the Frame
 */
  public SingleComponentFrame(Component c, String title) {
    super(title);
    constructorCommonCode(c,0,0, true);
  }
}

