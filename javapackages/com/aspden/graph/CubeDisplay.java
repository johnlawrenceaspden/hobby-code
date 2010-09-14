/*
 * CubeDisplay.java
 */

package com.aspden.graph;

import com.aspden.tespar.*;
import com.aspden.tespar.statstrategies.*;
import java.io.*;


/**
 */
public class CubeDisplay extends ButtonedTableDisplayer {

    /** Creates new CubeDisplay */
    public CubeDisplay(double[][][] cube, String[] rows, String[] columns, Colourizer colourizer, double brightest, double darkest) {
        super(rows, columns);
        for(int i=0; i<rows.length; i++)
        {
            for(int j=0; j<columns.length;j++)
            {
                this.add(new ColouredPatchCanvas(cube[i][j], brightest, darkest, colourizer), i, j);
            }
        }
    }
    
    public CubeDisplay(int [][][] cube, String[] rows, String[] columns, Colourizer colourizer)
    {
        super(rows, columns);
        for(int i=0; i<rows.length; i++)
        {
            for(int j=0; j<columns.length;j++)
            {
                int[] ranks=cube[i][j];
                double[] dranks=new double[ranks.length];
                for(int k=0; k<ranks.length; k++) dranks[k]=Math.pow(2,-ranks[k]);
                this.add(new ColouredPatchCanvas(dranks, 1, 0, colourizer), i, j);
            }
        }
    }

    /**
     * @param args the command line arguments
     */
    public static void main (String args[]) throws IOException{
        double[][][] cube=new double[][][]
        {
            {
                {  20,30,40
                },{ 50,80,30,20
                },{ 10,20,50,20,30,40
                }
            },{
                {    20,30,40
                },{  10,20,20,30,40,80,50,60,70
                },{  10,20,20,30,40
                }
            }

        };
new SingleComponentFrame(new CubeDisplay(cube,new String[]{"row 1","row 2"},new String[]{"column 1","column 2","column 3"}, new Colourizer.RedShade(), 0, 90),"Test cube");
    }

}

