/*
 * CrossComparison.java
 */

package com.aspden.tespar;

import com.aspden.tespar.statstrategies.*;
import com.aspden.tespar.statscomparison.*;
import com.aspden.tespar.utility.*;
import com.aspden.graph.*;

import java.io.*;

public class CrossComparison extends Object {

    /**
     * @param args the command line arguments
     */
    public static void main (String args[]) throws IOException{

        SpeechDatabase sd1=SpeechDatabase.MoreOguzDigitsSony11k.firstFiveGrabs();
        SpeechDatabase sd2=SpeechDatabase.OguzDigitsSony11k;
        
        StatisticsTable s2=new StatisticsTable(sd2.getName(),sd2.getRoot(), sd2.getSubdirs(), sd2.getFiles(), sd2.getFrequency(), new DefaultStrategy(), true);
        StatisticsTable s1=new StatisticsTable(sd1.getName(),sd1.getRoot(), sd1.getSubdirs(), sd1.getFiles(), sd1.getFrequency(), new DefaultStrategy(), true);


        new SingleComponentFrame(new HyperCubeDisplay(s2.crosshypercubeComparison(s1), s2.getDirectories(), s2.getFileNames(), new Colourizer.RedShade()), "Cross Hypercube");
        new SingleComponentFrame(new HyperCubeDisplay(s1.crosshypercubeComparison(s2), s1.getDirectories(), s1.getFileNames(), new Colourizer.RedShade()), "Cross Hypercube");

        double[][][] scores=s1.crossComparison(s2,(new CorrelateAgainstAverageComparator(new Archetype[0])));
        new SingleComponentFrame(new CubeDisplay(scores,s2.getDirectories(),s2.getFileNames(), new Colourizer.RedShade(), 0, 90), "Cross Comparison "+s2.getName()+" according to "+s1.getName());
        int [][][] ranks=Mathematics.scoresToRanks(scores);
        new SingleComponentFrame(new CubeDisplay(ranks, s2.getDirectories(),s2.getFileNames(), new Colourizer.GreyShade()), "Cross Comparison Ranks "+s2.getName()+" according to "+s1.getName()); 
        double[][] matrix1=Mathematics.ranksToMatrix(ranks, 0);
        new SingleComponentFrame(new ResultsPanel( s2.getFileNames(),s2.getFileNames(), matrix1, s2.getDirectories().length, -s2.getDirectories().length), "Cross Comparison first choice matrix "+s2.getName()+" according to "+s1.getName());
        double[][] matrix2=Mathematics.ranksToMatrix(ranks, 1);
        new SingleComponentFrame(new ResultsPanel( s2.getFileNames(),s2.getFileNames(), matrix2, s2.getDirectories().length, -s2.getDirectories().length), "Cross Comparison second choice matrix "+s2.getName()+" according to "+s1.getName());
        int[][] results=Mathematics.ranksToResults(ranks);
        new SingleComponentFrame(new ResultsPanel( s2.getDirectories(),s2.getFileNames(), results, 0, s2.getDirectories().length), "Recognition Ranks "+s2.getName()+" according to "+s1.getName());
        s2.highlightResults(results);
        
        
        double score = Mathematics.resultsToScore(results);
        System.out.println("\nScore="+score);
        double fom=Mathematics.resultsToFigureOfMerit(results);
        System.out.println("Figure Of Merit="+fom);
    }

}