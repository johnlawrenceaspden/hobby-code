/*
 * JackKnife.java
 */

package com.aspden.tespar;

import com.aspden.tespar.statstrategies.*;
import com.aspden.tespar.statscomparison.*;
import com.aspden.tespar.utility.*;
import com.aspden.graph.*;

import java.io.*;

public class JackKnife extends Object {

    private static void crossValidate(StatisticsTable s)
    {
        double[][][] scores=s.jackKnife(new CorrelateAgainstAverageComparator(new Archetype[0]));
        new SingleComponentFrame(new CubeDisplay(scores, s.getDirectories(), s.getFileNames(), new Colourizer.RedShade(), 0, 90), s.getName()+": jacknifed");
        int [][][] ranks=Mathematics.scoresToRanks(scores);
        new SingleComponentFrame(new CubeDisplay(ranks, s.getDirectories(), s.getFileNames(), new Colourizer.GreyShade()), s.getName()+": jacknifed and ranked");
        double[][] matrix1=Mathematics.ranksToMatrix(ranks, 0);
        new SingleComponentFrame(new ResultsPanel( s.getFileNames(),   s.getFileNames(), matrix1, s.getDirectories().length, -s.getDirectories().length), s.getName()+": jackknife first choice matrix");
        double[][] matrix2=Mathematics.ranksToMatrix(ranks, 1);
        new SingleComponentFrame(new ResultsPanel( s.getFileNames(),   s.getFileNames(), matrix2, s.getDirectories().length, -s.getDirectories().length), s.getName()+": jackknife second choice matrix");
        int[][] results=Mathematics.ranksToResults(ranks);
        new SingleComponentFrame(new ResultsPanel( s.getDirectories(),   s.getFileNames(), results, 0, s.getFileNames().length), s.getName()+": recognition rank vs self");
        s.highlightResults(results);


        double score = Mathematics.resultsToScore(results);
        System.out.println("\nScore="+score);
        double fom=Mathematics.resultsToFigureOfMerit(results);
        System.out.println("Figure Of Merit="+fom);
    }

    /**
     * @param args the command line arguments
     * @throws IOException If files not found
     */
    public static void main (String args[]) throws IOException {

        StatisticsTable s;
        if(false)
        {
            SpeechDatabase sd=SpeechDatabase.JamesTestDB;
            s = new StatisticsTable(sd.getName(),sd.getRoot(), sd.getSubdirs(), sd.getFiles(), sd.getFrequency(), new DefaultStrategy(), true);
        }
        else
        {
            String[] speakers = new String[]{"mac", "mae", "mal", "malx", "mam", "masma", "mbm",
                "mcad", "mcbh", "mce", "mcln", "mcly", "mcnd", "mcp", "mdjb", "mgln", "mgmr", "mimt", "mjbh",
            "mjsl", "mmhg", "mmw", "mogz", "molv", "mpte", "mrdh", "msa", "mstv", "mtcp", "mtes"};
            //String speaker=speakers[24];
            String speaker="mtes";
            s=new StatisticsTable(speaker,"C:\\ddldata", speaker,0 , new DefaultStrategy(), true);
        }




        crossValidate(s);

        new SingleComponentFrame(new HyperCubeDisplay(s.hypercubeComparison(),s.getDirectories(), s.getFileNames(),new Colourizer.RedShade()), s.getName()+": hypercube");
    }

}