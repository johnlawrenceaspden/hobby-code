/*
 * SquareDisplay.java
 */

package com.aspden.graph;

import com.aspden.tespar.*;
import com.aspden.tespar.statstrategies.*;
import java.io.*;


/**
 */
public class SquareDisplay extends ButtonedTableDisplayer {

    /** Creates new CubeDisplay */
    public SquareDisplay(double[][] square, String[] columns, Colourizer colourizer, double brightest, double darkest) {
        super(columns, columns);
        for(int i=0; i<columns.length; i++)
        {
            for(int j=0; j<columns.length;j++)
            {
                this.add(new ColouredPatchCanvas(square[i][j], brightest, darkest, colourizer), i, j);
            }
        }
    }

    /**
     * @param args the command line arguments
     */
    public static void main (String args[]) throws IOException{
        double[][] square=new double[][]
        {
            {  20,30,40
            },{ 50,80,30
            },{ 10,20,50
            }
        };
new SingleComponentFrame(new SquareDisplay(square,new String[]{"column 1","column 2","column 3"}, new Colourizer.RedShade(), 0, 90), "Test square");
    }

}

