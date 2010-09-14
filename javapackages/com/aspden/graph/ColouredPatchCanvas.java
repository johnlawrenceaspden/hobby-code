/*
 * ColouredPatchCanvas.java
 */

package com.aspden.graph;

import java.awt.*;
import com.aspden.graphwidget.misc.*;


/**
 */
public class ColouredPatchCanvas extends Canvas {
    private double[][] data;
    private Colourizer theColourizer;

    public void constructorCommonCode(double[][] data, double brightest, double darkest, Colourizer colourizer) {
        this.theColourizer=colourizer;
        this.data=new double[data.length][];
        for(int i=0;i<data.length;i++)
        {
            this.data[i]=new double[data[i].length];
            for(int j=0;j<data[i].length;j++)
            {
                this.data[i][j]=(data[i][j]-darkest)/(brightest-darkest);
            }
        }
    }
    
    public ColouredPatchCanvas(double[][] data, double brightest, double darkest, Colourizer colourizer) {
        constructorCommonCode(data,brightest,darkest,colourizer);
    }
    
    public ColouredPatchCanvas(double[][] data, Colourizer colourizer) {
        double min=data[0][0],max=data[0][0];
        for(int i=0;i<data.length;i++)
        {
            for(int j=0;j<data[i].length;j++)
            {
                if(data[i][j]<min) min=data[i][j];
                if(data[i][j]>max) max=data[i][j];
            }
        }
        constructorCommonCode(data,max,min,colourizer);
    }
    
    public ColouredPatchCanvas(double[] data, double brightest, double darkest, Colourizer colourizer) {
        this(new double[][]{data},brightest,darkest, colourizer);
    }
    
    public ColouredPatchCanvas(double data, double brightest, double darkest, Colourizer colourizer) {
        this(new double[][]{{data}},brightest,darkest, colourizer);
    }

    
    public void paint(Graphics g)
    {
        for(int i=0;i<data.length;i++)
        {
            double dy=this.getBounds().height/(data.length);
            for(int j=0;j<data[i].length;j++)
            {
                double dx=this.getBounds().width/(data[i].length);
                g.setColor(theColourizer.getColour(data[i][j]));
                g.fill3DRect((int)(j*dx), (int)(i*dy), (int)dx,(int)dy,true);
            }
        }
    }
    
    public Dimension getPreferredSize()
    {
        if (data.length==0) return new Dimension(15,15);
        else if (data[0].length==0) return new Dimension(15,15);
        else return new Dimension(data[0].length*15,data.length*15);
    }

    /**
     * @param args the command line arguments
     */
    public static void main (String args[]) {
        double[][] scores={{0.73, 1.01}, {0.3}, {0.93, 0.27}};
        new SingleComponentFrame(new ColouredPatchCanvas(scores, 0, 1, new Colourizer.GreyShade()), "ColouredPatchCanvas Test") ;
        new SingleComponentFrame(new ColouredPatchCanvas(scores, new Colourizer.GreyShade()), "ColouredPatchCanvas Test AutoScale") ;
        new SingleComponentFrame(new ColouredPatchCanvas(new double[] {0.1,0.2,0.3}, 0, 1, new Colourizer.GreyShade()), "ColouredPatchCanvas 1d Test") ;
        new SingleComponentFrame(new ColouredPatchCanvas(0.1, 0, 1, new Colourizer.GreyShade()), "ColouredPatchCanvas 0d Test") ;
    }

}