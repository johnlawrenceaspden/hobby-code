/*
 * SoundWaveTable.java
 */

package com.aspden.tespar;

import com.aspden.tespar.basics.*;
import com.aspden.graph.*;
import com.aspden.widgets.*;
import java.util.*;
import java.io.*;
import java.text.*;
import java.awt.*;

/** Represents the and manipulates the usual structure of named .txt files in separate subdirectories.
 * e.g. record00 contains {6.txt, 7.txt, z.txt}
 *      record01 contains {9.txt, 6.txt}
 */
public class SoundWaveTable extends Object {
    AssociativeArray2D waves=new AssociativeArray2D();
    Panel theDisplay;
    File rootdir;
    
    private static final int FREQUENCY=11025;

    private FilenameFilter txtfiles=new FilenameFilter(){ //filter corresponding to "*.tzt"
        public boolean accept(File dummy, String s){
            return(s.endsWith(".txt"));
        }
    };
    /** Creates new SoundwaveTable representing the structure under a given directory.
     * @param rootdir The root of the soundwave tree structure
     * @throws IOException When the files cannot be read for some reason.
     */
    public SoundWaveTable(File rootdir) throws IOException{
        if(!rootdir.isDirectory()){
            throw new IllegalArgumentException("Must be a directory.");
        }
        this.rootdir=rootdir;
        File[] subdirs=rootdir.listFiles();
        for(int i=0; i<subdirs.length; i++)
        {
            if(subdirs[i].isDirectory())
            {
                File[] files=(subdirs[i]).listFiles(txtfiles);
                for(int j=0; j<files.length; j++)
                {
                    waves.add(subdirs[i].getName(),files[j].getName(),new SoundWave(files[j], FREQUENCY));
                }
            }
        }
    }

    public void add(SoundWave s, String name)
    {
        int i=0;
        //find the first directory that doesn't contain a file 'name'
        while(waves.get(subdirgenerator(i),name)!=null) i++;
        //add the soundwave to our table in that directory.
        waves.add(subdirgenerator(i), name, s);
        //ensure that directory physically exists
        File subdir=new File(rootdir, subdirgenerator(i));
        if(!subdir.exists()) subdir.mkdir();
        //write the file to disk in its new position
        File newfile=new File(subdir, name);
        s.writeToFile(newfile);

        updateDisplay();
    }


    private String subdirgenerator(int i)
    {
        DecimalFormat f=new DecimalFormat("00");
        return "record"+f.format(i);

    }

    private void updateDisplay()
    {
        if(theDisplay!=null)
        {
            String[] rows=waves.getRows();
            String[] columns=waves.getColumns();
            Arrays.sort(rows);
            Arrays.sort(columns);
            ButtonedTableDisplayer b=new ButtonedTableDisplayer(rows , columns);
            for(int i=0; i<rows.length; i++)
            {
                for(int j=0; j<columns.length; j++){
                    SoundWave s=(SoundWave) waves.get(rows[i], columns[j]);
                    if(s!=null) b.add(s.getDisplay(),i,j);
                }
            }
            theDisplay.removeAll();
            theDisplay.add(b);
            theDisplay.validate();
        }
    }

    public Component getDisplay()
    {
        if(theDisplay==null)
        {
            theDisplay=new Panel();
            theDisplay.setLayout(new BorderLayout());
        }
        updateDisplay();
        return theDisplay;
    }
}
