/*
 * StatisticsTable.java
 */

package com.aspden.tespar;

import com.aspden.tespar.statistics.*;
import com.aspden.tespar.statstrategies.*;
import com.aspden.tespar.statscomparison.*;
import com.aspden.tespar.basics.*;
import com.aspden.graph.*;


import java.util.*;
import java.io.*;
import java.awt.*;
import java.util.List;

/** A two dimensional collection of TESPAR {@link Statistic}s:
 * <I>e.g.</I> Suppose there exist three subdirectories record01, record02, & record03,
 * and each contains files apple.txt, pear.txt, and banana.txt.
 * We create a StatisticsTable using a particular {@link StatisticAcquisitionStrategy},
 * and it then behaves like an array of derived Statistics, indexed first by subdirectory and then by filename.
 * If desired, any visual components produced by the strategy illustrating the processing that has happened
 * can be collected and displayed as arrays in top level frames, using TableDisplayer objects.
 * Statistics in the Table can be highlighted so that their displays stand out.
 */
public class StatisticsTable extends Object {
    private Statistic[][] theTable;
    private String[] filenames;
    private String[] directories;
    private List theTableDisplayers;
    private String tableTitle;
    private int frequency;

    /** Create a table of TESPAR Statistics given a tree of sound files.
     * @param dataDirectory The top level directory of the speech database, e.g. "C:\speechdatabase\john\digits".
     * @param grabDirectories The subdirectories that hold the various grabs, e.g. {"record01", "record02", ...}
     * @param filenames The names of the sound files, e.g. {"1.txt", "2.txt", "01223 526562.txt"}
     * @param theStrategy The strategy to be used to convert sound files into Statistics.
     * @param tableTitle A title to be used on displays created by this table.
     * @param displays Whether or not to display the graphs returned in the processing.
     * @throws IOException If there is a problem reading the specified files.
     */
    public StatisticsTable(String tableTitle, String dataDirectory, String[] grabDirectories,String[] filenames, int frequency, StatisticAcquisitionStrategy theStrategy, boolean displays) throws IOException {
        this.tableTitle=tableTitle;
        this.filenames=filenames;
        this.directories=grabDirectories;
        this.frequency=frequency;

        //Initialise the 2D array to hold one statistic for each file in each directory.
        theTable=new Statistic[directories.length][];
        for(int i=0; i<directories.length; i++)
        {
            theTable[i]=new Statistic[filenames.length];
        }

        List titles=new ArrayList();

        //Fill the table with statistics processed by the strategy.
        for(int i=0;i<directories.length;i++)
        {
            //processing a directory at a time e.g. record01
            String directory=dataDirectory+File.separator+directories[i];
            System.out.print("\nreading "+directory);
            //iterate over particular filenames in that directory
            for(int j=0; j<filenames.length; j++)
            {
                String filename=filenames[j];
                System.out.print(" "+filename);

                titles=new ArrayList();
                List components=new ArrayList(); //to hold return graphics from the strategy.
                theTable[i][j]=theStrategy.getStatistic(new SoundWave(new File(directory+File.separator+filename), frequency), titles, components);

                if(displays) addToDisplays(i, j, titles, components);
            }
        }

        if(displays) activateDisplays(titles);
    }


    /**Creates a table for one of the speakers recorded by DDL*/
    public StatisticsTable(String tableTitle, String dataDirectory, String prefix, int noisetype, StatisticAcquisitionStrategy theStrategy, boolean displays) throws IOException{
        this.tableTitle=tableTitle;
        this.filenames=new String[]{"d0","d1","d2","d3","d4","d5","d6","d7","d8","d9","doh"};
        this.directories=new String[]{"s1", "s2", "s3", "s4", "s5"};

        //Initialise the 2D array to hold one statistic for each file in each directory.
        theTable=new Statistic[directories.length][];
        for(int i=0; i<directories.length; i++)
        {
            theTable[i]=new Statistic[filenames.length];
        }

        List titles=new ArrayList();

        //Fill the table with statistics processed by the strategy.
        for(int i=0;i<directories.length;i++)
        {
            //processing a directory at a time e.g. record01
            String directory=dataDirectory+File.separator+prefix+"_n"+noisetype+directories[i];
            System.out.print("\nreading "+directory);
            //iterate over particular filenames in that directory
            for(int j=0; j<filenames.length; j++)
            {
                String filename=filenames[j];
                System.out.print(" "+filename);

                titles=new ArrayList();
                List components=new ArrayList(); //to hold return graphics from the strategy.
                theTable[i][j]=theStrategy.getStatistic(new SoundWave(new File(directory+filename+".wav")), titles, components);

                if(displays) addToDisplays(i, j, titles, components);
            }
        }

        if(displays) activateDisplays(titles);
    }

    
    
    
    
    
    
    
    
    
    /**Activate all the associated TableDisplayers*/
    private void activateDisplays(List titles)
    {
        if(theTableDisplayers!=null)
        {
            Iterator tables=theTableDisplayers.iterator();
            Iterator ititles=titles.iterator();
            while(tables.hasNext())
            {
                new SingleComponentFrame(((Component)tables.next()), tableTitle+" : "+(String)ititles.next(),0.5,0.75);
            }
        }
    }

    /**Given the lists of graphics produced by the strategy (which should be similar for each table entry), add the graphics to appropriate TableDisplayers, creating as necessary.*/
    private void addToDisplays(int directory, int filename, List titles, List components)
    {
        //if we haven't set the table displayers up yet do so
        if(theTableDisplayers==null)
        {
            theTableDisplayers=new ArrayList();
            Iterator ititles=titles.iterator();
            while(ititles.hasNext())
            {
                String title=(String) ititles.next();
                TableDisplayer t=new ButtonedTableDisplayer(directories, filenames);
                theTableDisplayers.add(t);
            }
        }

        //Add components to the appropriate TableDisplayer
        Iterator component=components.iterator();
        Iterator tables=theTableDisplayers.iterator();
        while(component.hasNext())
        {
            java.awt.Component c=(java.awt.Component)component.next();
            TableDisplayer t=(TableDisplayer)tables.next();
            t.add(c, directory, filename);
        }
    }

    /** Get the statistic associated with a particular file.
     * @return The statistic derived from the file.
     * @param directory index of the directory in the list when the StatisticsTable was created.
     * @param file index of the filename in the list of names.
     */
    public Statistic getStatistic(int directory,int file) {
        return theTable[directory][file];
    }

    /** Cause any displays created by this StatisticsTable to highlight
     * the component relating to a particular statistic.
     * @param directory directory index of Statistic to be highlighted.
     * @param file file index.
     */
    public void highlight(int directory,int file) {
        if(theTableDisplayers!=null){
            for(Iterator it= theTableDisplayers.iterator();it.hasNext();)
            {
                ((TableDisplayer) it.next()).highlight(directory, file);
            }
        }
    }

    /** Retrieve the directory names.
     * @return the directory names
     */
    public String[] getDirectories() {
        return this.directories;
    }

    /** Retrieve the filenames.
     * @return the filenames.
     */
    public String[] getFileNames() {
        return this.filenames;
    }

    /** Compares a given Statistic individually with every Statistic in the table.
     * Returns the result of the Statistic's correlation method used to compare the given statistic with the one in the Table.
     * @param s The Statistic to be compared with the Table
     * @return The matrix of correlation scores.
     */
    public double[][] compare(Statistic s)
    {
        double[][] scores=new double[this.directories.length][];
        for(int i=0; i<this.directories.length; i++)
        {
            scores[i]=new double[this.filenames.length];
            for(int j=0; j<this.filenames.length; j++)
            {
                scores[i][j]=this.getStatistic(i,j).correlate(s);
            }
        }
        return scores;
    }

    /** returns the result of the {@link compare} method for every Statistic in the table.
     * This results in a four-dimensional array.
     * @return array of comparison scores. element [i][j][k][l] is the result of the comparison between the element derived from directory i, file j with that from directory k, file l.
     */
    public double[][][][] hypercubeComparison()
    {
        double hypercube[][][][]=new double[this.directories.length][][][];
        for(int i=0; i<this.directories.length; i++)
        {
            hypercube[i]=new double[this.filenames.length][][];
            for(int j=0; j<this.filenames.length; j++)
            {
                hypercube[i][j]=compare(this.getStatistic(i,j));
            }
        }
        return hypercube;
    }

    public double[][][][] crosshypercubeComparison(StatisticsTable st)
    {
        double hypercube[][][][]=new double[this.directories.length][][][];
        for(int i=0; i<this.directories.length; i++)
        {
            hypercube[i]=new double[this.filenames.length][][];
            for(int j=0; j<this.filenames.length; j++)
            {
                hypercube[i][j]=st.compare(this.getStatistic(i,j));
            }
        }
        return hypercube;
    }

    public double[][][] crossComparison(StatisticsTable target, StatComparator prototype)
    {
        //Create an archetype for each of the file types in this table
        int thiscolumns=this.filenames.length;
        int thisrows=this.directories.length;
        Archetype[] archetypes=new Archetype[thiscolumns];
        for(int file=0; file<thiscolumns; file++)
        {
            archetypes[file]=new Archetype();
            for(int dir=0; dir<thisrows; dir++)
            {
                archetypes[file].add(theTable[dir][file]);
            }
        }

        //Create a comparator like the prototype that has been passed in.
        StatComparator sc=prototype.getInstance(archetypes);

        //now compare every statistic in the target with the comparator.
        int targetrows=target.directories.length;
        int targetcols=target.filenames.length;
        double[][][] scores=new double[targetrows][][];
        for(int dir=0; dir<targetrows; dir++)
        {
            scores[dir]=new double[targetcols][];
            for(int file=0; file<targetcols; file++)
            {
                scores[dir][file]=sc.getScores(target.theTable[dir][file]);
            }

        }
        return scores;
    }

    /** The strange operation of jackknifing (or cross-validation).
     * For each row, archetypes of all the other rows are made.
     * Then each element of the row is compared against each archetype.
     * The result is a three dimensional array.
     * @param prototype An example of the comparator which will be used to compare Statistic against Archetype.
     * @return The three dimensional result array.
     * Element [i][j][a] is the result of comparing
     * statistic [i][j] against an archetype made from all
     * the statistics in column a, except the one in row i.
     */
    public double[][][] jackKnife(StatComparator prototype)
    {
        double[][][] scores;

        int rows=theTable.length;
        scores=new double[rows][][];
        for(int testdir=0; testdir<rows; testdir++)
        {
            //create an array of archetypes, one for each column of the table
            int columns=theTable[testdir].length;
            scores[testdir]=new double[columns][];
            Archetype[] archetypes=new Archetype[columns];

            //make each archetype up from all the files of this type which are not in the test set
            for(int file=0; file<columns; file++)
            {
                archetypes[file]=new Archetype();
                for(int dir=0; dir<rows; dir++)
                {
                    if(dir!=testdir) archetypes[file].add(theTable[dir][file]);
                }
            }
            //now compare every statistic in the test row with the archetypes we've just created.
            //Using a comparator like the prototype that has been passed in.

            StatComparator sc=prototype.getInstance(archetypes);
            for(int file=0; file<columns; file++)
            {
                scores[testdir][file]=sc.getScores(theTable[testdir][file]);
            }

        }
        return scores;
    }

    /** Retrieve the name given to the Table on creation, which adorns any graphs it may produce.
     * @return the name.
     */
    public String getName()
    {
        return tableTitle;
    }

    public void highlightResults(int[][] results)
    {
        for(int i=0; i<results.length; i++)
        {
            for(int j=0; j<results[i].length; j++)
            {
                if(results[i][j]!=0) highlight(i,j);
            }
        }
    }

    /** Test/Demo code
     * @param args ignored.
     * @throws IOException If there are problems reading the databases.
     */
    public static void main(String[] args) throws IOException{
        //SpeechDatabase sd=SpeechDatabase.JamesTestDB;
        //StatisticsTable s = new StatisticsTable(sd.getName(),sd.getRoot(), sd.getSubdirs(), sd.getFiles(), sd.getFrequency(), new DefaultStrategy(), true);
        StatisticsTable s=new StatisticsTable("mac","C:\\ddldata", "mac", 0, new DefaultStrategy(), true);
        s.highlight(0,0);
        s.highlight(s.getDirectories().length-1, s.getFileNames().length-1);
        new SingleComponentFrame(new CubeDisplay(s.jackKnife(new CorrelateAgainstAverageComparator(new Archetype[0])),s.getDirectories(),s.getFileNames(), new Colourizer.RedShade(), 0, 90), "JackKnife "+s.getName());
        new SingleComponentFrame(new HyperCubeDisplay(s.hypercubeComparison(),s.getDirectories(),s.getFileNames(), new Colourizer.RedShade()), "Hypercube "+s.getName());
    }
}