/*
 * HyperCubeDisplay.java
 */

package com.aspden.graph;

import com.aspden.tespar.*;
import com.aspden.tespar.statstrategies.*;
import java.io.*;


/**
 */
public class HyperCubeDisplay extends ButtonedTableDisplayer {
    
    /** Creates new HyperCubeDisplay */
    public HyperCubeDisplay(double[][][][] tess, String[] rows, String[] columns, Colourizer colourizer) {
        super(rows, columns);
        for(int i=0; i<rows.length; i++)
        {
            for(int j=0; j<columns.length;j++)
            {
                this.add(new ColouredPatchCanvas(tess[i][j], 0,90, colourizer), i,j);
            }
        }
    }
    
    /**
     * @param args the command line arguments
     */
    public static void main (String args[]) throws IOException{
        double[][][][] tess=new double[][][][]
        {
            {
                {
                    {10,20},{20,30,40}
                },{
                    {50,80},{30,20}
                },{
                    {10,20,50},{20,30,40}
                }
            },{
                {
                    {20
                    },{30,40
                    }
                },{
                    {10,20
                    },{20,30,40,80
                    },{50,60,70
                    }
                },{
                    {10,20
                    },{20,30,40
                    }
                }
            }
        };
        new SingleComponentFrame(new HyperCubeDisplay(tess,new String[]{"row 1","row 2"},new String[]{"column 1","column 2","column 3"}, new Colourizer.RedShade()),"Test hypercube");
        new SingleComponentFrame(new HyperCubeDisplay(tess,new String[]{"row 1","row 2"},new String[]{"column 1","column 2","column 3"}, new Colourizer.GreyShade()),"Test hypercube");
        new SingleComponentFrame(new HyperCubeDisplay(tess,new String[]{"row 1","row 2"},new String[]{"column 1","column 2","column 3"}, new Colourizer.LogRainbow()),"Test hypercube");
        new SingleComponentFrame(new HyperCubeDisplay(tess,new String[]{"row 1","row 2"},new String[]{"column 1","column 2","column 3"}, new Colourizer.Palette()),"Test hypercube");
    }
    
}

