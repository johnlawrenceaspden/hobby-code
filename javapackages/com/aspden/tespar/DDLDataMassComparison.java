/*
 * DDLDataMassComparison.java
 */

package com.aspden.tespar;

import com.aspden.tespar.statscomparison.*;
import com.aspden.tespar.statstrategies.*;
import com.aspden.tespar.utility.*;
import com.aspden.graph.*;

import java.awt.*;
import java.text.*;


public class DDLDataMassComparison extends Object {

    private static double crossValidate(StatisticsTable s)
    {
        double[][][] scores=s.jackKnife(new CorrelateAgainstAverageComparator(new Archetype[0]));
        //new SingleComponentFrame(new CubeDisplay(scores, s.getDirectories(), s.getFileNames(), new Colourizer.RedShade(), 0, 90), s.getName()+": jacknifed");
        int [][][] ranks=Mathematics.scoresToRanks(scores);
        //        new SingleComponentFrame(new CubeDisplay(ranks, s.getDirectories(), s.getFileNames(), new Colourizer.GreyShade()), s.getName()+": jacknifed and ranked");
        //        double[][] matrix1=Mathematics.ranksToMatrix(ranks, 0);
        //        new SingleComponentFrame(new ResultsPanel( s.getFileNames(),   s.getFileNames(), matrix1, s.getDirectories().length, -s.getDirectories().length), s.getName()+": jackknife first choice matrix");
        //        double[][] matrix2=Mathematics.ranksToMatrix(ranks, 1);
        //        new SingleComponentFrame(new ResultsPanel( s.getFileNames(),   s.getFileNames(), matrix2, s.getDirectories().length, -s.getDirectories().length), s.getName()+": jackknife second choice matrix");
        int[][] results=Mathematics.ranksToResults(ranks);
        s.highlightResults(results);


        double score = Mathematics.resultsToScore(results);
        System.out.println("\nScore="+score);
        double fom=Mathematics.resultsToFigureOfMerit(results);
        System.out.println("Figure Of Merit="+fom);
        Panel p=new Panel();
        p.add(new ResultsPanel( s.getDirectories(),   s.getFileNames(), results, 0, s.getFileNames().length));

        NumberFormat f=NumberFormat.getPercentInstance();
        f.setMinimumFractionDigits(2);

        List a=new List(2);
        a.setFont(new Font("Helvetica", Font.PLAIN, 28));
        a.add(f.format(score));
        a.add("\n"+f.format(fom));
        p.add(a);
        new SingleComponentFrame(p, s.getName()+": recognition rank vs self");
        return score;
    }

    /**
     * @param args the command line arguments
     */
    public static void main (String args[]) throws java.io.IOException{
        int noiselevel=0;
        TextArea theList=new TextArea();
        theList.append("DDL Database noise level " +noiselevel);
        NumberFormat f=NumberFormat.getPercentInstance();
        f.setMinimumFractionDigits(0);
        new SingleComponentFrame(theList);
        
        
        
        String[] speakers = new String[]{"mac", "mae", "mal", "malx", "mam", "masma", "mbm",
            "mcad", "mcbh", "mce", "mcln", "mcly", "mcnd", "mcp", "mdjb", "mgln", "mgmr", "mimt", "mjbh",
        "mjsl", "mmhg", "mmw", "mogz", "molv", "mpte", "mrdh", "msa", "mstv", "mtcp", "mtes"};
        for(int i=0; i<speakers.length; i++)
        {
            StatisticsTable s=new StatisticsTable(speakers[i],"C:\\ddldata", speakers[i], noiselevel, new DefaultStrategy(), false);
            double score=crossValidate(s);
            theList.append("\n"+speakers[i]+" "+f.format(score));
        }
    }

}