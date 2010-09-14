package com.aspden.graphwidget.misc;


import java.awt.event.*;
import java.awt.*;

/**
 * Frame which responds to close messages by shutting down
 * the entire application
 */
public class CloseableFrame extends Frame{

    /** Creates a Frame which will kill the system when closed.
     * @param title The Title of the new Frame.
     */
    public CloseableFrame(String title)
    {
        super(title);
        addWindowListener(new WindowAdapter(){
            public void windowClosing(WindowEvent e){
            System.exit(0);}
        });
    }

}
