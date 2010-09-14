/*
 * Mathematics.java
 */

package com.aspden.tespar.utility;


import com.aspden.graph.*;
import com.aspden.*;


/**A spectral class which encapsulates some mathematical operations.*/
public class Mathematics extends Object
{
    /** Given a list of scores, return a corresponding list of rankings.
     * Low scores mean low ranks.
     * @param scores the scores, e.g. 23, 45, 78
     * @return the ranks, e.g 0, 1, 2
     */
    public static int[] scoresToRanks(double[] scores)
    {
        double[] sc=(double[])scores.clone();
        int[] ranks=new int[sc.length];
        boolean[] counted=new boolean[sc.length];
        for(int rank=sc.length-1; rank>=0; rank--)
        {
            double max=Double.NEGATIVE_INFINITY;
            int maxindex=-1;
            for(int i=0; i<sc.length; i++)
            {
                if(!counted[i])
                {
                    if(sc[i]>max)
                    {
                        max=sc[i];maxindex=i;
                    }
                }
            }
            ranks[maxindex]=rank;
            counted[maxindex]=true;

        }
        return ranks;
    }

    /** Apply the scores to ranks operation to a two dimensional table of scores.
     * @param scores the table of scores. each scores[i][j] is a set of scores suitable as an argument for int[] scoresToRanks(double[]) above
     * @return a 2D array of rankings (lowest score-> rank 0)
     */
    public static int[][][] scoresToRanks(double[][][] scores)
    {

        int[][][] ranks=new int[scores.length][][];
        for(int i=0;i<scores.length;i++)
        {
            ranks[i]=new int[scores[i].length][];
            for(int j=0;j<scores[i].length;j++)
            {
                ranks[i][j]=Mathematics.scoresToRanks(scores[i][j]);
            }
        }
        return ranks;
    }


    /** Find the lowest score in a list.
     * @param scores scores to be examined.
     * @return index of lowest score in list.
     */
    public static int lowest(double[] scores)
    {
        int[] ranks=scoresToRanks(scores);
        int i=0;
    while(ranks[i]!=0) {i++;}
        return i;
    }

    /** Calculate the (canonical) inner product of two vectors.
     * @param a a vector
     * @param b another vector of the same length
     * @return a.b =a[0]*b[0]+a[1]*b[1]+.....+a[n]*b[n]
     */
    public static double innerProduct(double[] a, double[] b)
    {
        if(a.length!=b.length) throw new IllegalArgumentException("Tried to take inner product of differently sized arrays");

        double ip=0;
        for(int i=0; i<a.length; i++)
        {
            ip+=a[i]*b[i];
        }

        return ip;
    }

    public static double angleInDegrees(double cosTheta)
    {
        double x=Math.acos(cosTheta);
        x/=Math.PI;
        x*=180;
        return x;
    }


    public static double cosTheta(double[] a, double[] b)
    {
        double ip = innerProduct(a,b);
        double cosTheta = Math.sqrt(ip*ip/innerProduct(a,a)/innerProduct(b,b));
        if(cosTheta>1.000001 || cosTheta<-1.000001) throw new IllegalArgumentException("cos theta calculation gone haywire costheta="+cosTheta);
        if(cosTheta>1.0) cosTheta=1.0;
        if (cosTheta<-1.0) cosTheta=-1.0;
        return cosTheta;
    }

    /** L1 distance between two vectors.
     * @param a a vector
     * @param b another vector of the same length
     * @return |a-b| =|a[0]-b[0]|+|a[1]-b[1]|+.....+|a[n]-b[n]|
     */
    public static double distanceL1(double a[], double b[])
    {
        if(a.length!=b.length) throw new IllegalArgumentException("Tried to compare differently sized arrays");
        double diff=0;
        for(int i=0; i<a.length; i++)
        {
            diff+=Math.abs(a[i]-b[i]);
        }
        return diff;
    }

    /** L infinity distance between two vectors.
     * @param a a vector
     * @param b another vector of the same length
     * @return |a-b| =max(|a[0]-b[0]|,|a[1]-b[1]|,.....,|a[n]-b[n]|)
     */
    public static double distanceLinfinity(double a[], double b[])
    {
        if(a.length!=b.length) throw new IllegalArgumentException("Tried to compare differently sized arrays");
        double diff=0;
        for(int i=0; i<a.length; i++)
        {
            double d=Math.abs(a[i]-b[i]);
            if(d>diff) diff=d;
        }
        return diff;
    }

    public static double[][] ranksToMatrix(int ranks[][][], int level)
    {
        int size=ranks[1].length;
        double[][] matrix=new double[size][size];
        for(int i=0; i<ranks.length; i++)
        {
            for(int j=0; j<size; j++)
            {
                for(int k=0; k<size; k++)
                {
                    if(ranks[i][j][k]==level) matrix[j][k]++;
                }
            }
        }
        return matrix;
    }

    public static double resultsToScore(int results[][])
    {
        return score(results, false);
    }

    public static double resultsToFigureOfMerit(int results[][])
    {
        return score(results, true);
    }

    private static double score(int results[][], boolean figureOfMerit)
    {
        int columns=results[1].length;
        int total=0;
        int score=0;
        double fom=0.0;
        for(int i=0; i<results.length; i++)
        {
            for(int j=0; j<columns; j++)
            {
                total++;
                if(results[i][j]==0) score++;
                fom+=Math.pow(2, -results[i][j]);
            }
        }
        if(figureOfMerit) return fom/total;
        else return ((double)score)/total;
    }

    public static int[][] ranksToResults(int ranks[][][])
    {
        int[][] results=new int[ranks.length][];
        for(int i=0; i<ranks.length; i++)
        {
            results[i]=new int[ranks[i].length];
            for(int j=0; j<ranks[i].length; j++)
            {
                results[i][j]=ranks[i][j][j];
            }
        }
        return results;
    }
    
    public static double[] chop(int begin, int end, double[] d)
    {
        if(end<begin) end=begin;
        double[] newdata=new double[end-begin+1];
        for(int i=begin; i<end; i++)
        {
            newdata[i-begin]=d[i];
        }
        return newdata;
    }
    
    public static double[] calculateCumulativeMass(double[] data)
    {
        double[] energy=new double[data.length];
        double mass=0;

        for(int i=0; i<data.length; i++)
        {
            mass+=Math.abs(data[i]);
            energy[i]=mass;
        }

        return energy;

    }
    
    public static double[] filter3ptAverage(double[] data)
    {
        double[] filtered=new double[data.length];
        filtered[0]=data[0];
        filtered[data.length-1]=data[data.length-1];
        for(int i=1; i<data.length-1; i++) filtered[i] = (data[i-1]+data[i]+data[i+1])/3;
        return filtered;
    }


    public static double[] sorted(double[] a)
    {
        double[] sorted=(double[])(a.clone());
        java.util.Arrays.sort(sorted);
        return sorted;
    }
    
    public static double[] calculateL2Energy(int windowsize, double[] data)
    {
        double[] energy=new double[data.length];

        double ensquared=0;
        int halfwindow=windowsize/2;

        if(windowsize>data.length) //The wave doesn't have enough samples for this calculation to be meaningful
        {
            System.out.println("NOT ENOUGH DATA FOR ENERGY CALCULATION!!");
            return energy;
        }

        for(int i=0; i<windowsize; i++) ensquared+=data[i]*data[i];
        for(int i=0; i<halfwindow; i++) energy[i]=(ensquared/windowsize);
        for(int i=halfwindow; i<data.length-halfwindow; i++)
        {
            ensquared+=data[i+halfwindow]*data[i+halfwindow];
            ensquared-=data[i-halfwindow]*data[i-halfwindow];
            energy[i]=(ensquared/windowsize);
        }
        for(int i=data.length-halfwindow; i<data.length; i++) energy[i]=(ensquared/windowsize);
        
        for(int i=0; i<data.length; i++) energy[i]=Math.sqrt(energy[i]);

        return energy;
    }
    
    public static double[] calculateH1Energy(int windowsize, double[] data)
    {
        double[] energy=new double[data.length];

        double ensquared=0;
        int halfwindow=windowsize/2;

        if(windowsize>data.length) //The wave doesn't have enough samples for this calculation to be meaningful
        {
            System.out.println("NOT ENOUGH DATA FOR ENERGY CALCULATION!!");
            return energy;
        }

        for(int i=1; i<windowsize+1; i++) ensquared+=data[i]*data[i]+(data[i]-data[i-1])*(data[i]-data[i-1]);
        for(int i=0; i<=halfwindow+1; i++) energy[i]=(ensquared/windowsize);
        for(int i=halfwindow+1; i<data.length-halfwindow; i++)
        {
            ensquared+=data[i+halfwindow]*data[i+halfwindow]+(data[i+halfwindow]-data[i+halfwindow-1])*(data[i+halfwindow]-data[i+halfwindow-1]);
            ensquared-=data[i-halfwindow]*data[i-halfwindow]+(data[i-halfwindow]-data[i-halfwindow-1])*(data[i-halfwindow]-data[i-halfwindow-1]);
            energy[i]=(ensquared/windowsize);
        }
        for(int i=data.length-halfwindow; i<data.length; i++) energy[i]=(ensquared/windowsize);
        
        for(int i=0; i<data.length; i++) energy[i]=Math.sqrt(energy[i]);

        return energy;
    }
    
    public static int findFirstPointAbove(double threshold, double[] energy)
    {
        int begin=0;
        while(begin<energy.length)
        {
            if (energy[begin]>threshold) break;
            begin++;
        }
        return begin;
    }
    
    public static int findLastPointAbove(double threshold, double[] energy)
    {
        int end=energy.length-1;
        while(end>=0)
        {
            if (energy[end]>threshold) break;
            end--;
        }
        return end;
    }





    /** Demo/Test code.
     * @param args ignored.
     */
    public static void main(String[] args) {
    double[] scores = {80,95,23,95,36,72};
    double[] a = {80,95,23,95,36,72};
    double[] b = {2,95,5,95,23,65};
        System.out.println("scores= "+Stuff.stringify(scores));
        System.out.println("ranks=  "+Stuff.stringify(scoresToRanks(scores)));
        System.out.println("lowest= "+scores[lowest(scores)]+" index="+lowest(scores));
        System.out.println();

        System.out.println("a="+Stuff.stringify(a));
        System.out.println("b="+Stuff.stringify(b));
        System.out.println("inner product "+innerProduct(a,b));
        System.out.println("cos theta "+cosTheta(a,b));
        System.out.println("angle in degrees "+angleInDegrees(cosTheta(a,b)));
        System.out.println("L1 distance "+distanceL1(a,b));
        System.out.println("L infinity distance "+distanceLinfinity(a,b));

    String[] rows= new String[]{"grab1","grab2","grab3","grab4"};
    String[] columns = new String[]{"a","b","c"};
        double[][][] scoreset={
            {
    {16,43,12},{56,10,43},{92,87,3}
            },
            {
    {20,53,12},{54,7,20},{70,29,34}
            },
            {
    {16,43,12},{56,10,43},{89,87,3}
            },
            {
    {16,43,12},{56,10,43},{92,87,3}
            }
        };
        new SingleComponentFrame(new CubeDisplay(scoreset, rows, columns, new Colourizer.RedShade(), 0, 90), "Test Scores");
        int[][][] ranks=scoresToRanks(scoreset);
        new SingleComponentFrame(new CubeDisplay(ranks, rows, columns, new Colourizer.RedShade()), "Test Ranks");
        double[][] matrix=ranksToMatrix(ranks, 0);
        new SingleComponentFrame(new SquareDisplay(matrix, columns, new Colourizer.GreyShade(), ranks.length, 0), "Test Matrix for rank 0");
        double[][] matrix2=ranksToMatrix(ranks, 1);
        new SingleComponentFrame(new SquareDisplay(matrix2, columns, new Colourizer.GreyShade(), ranks.length, 0), "Test Matrix for rank 1");
    }
}
