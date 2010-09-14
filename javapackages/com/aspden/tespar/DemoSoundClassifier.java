/*
 * DemoSoundClassifier.java
 */

package com.aspden.tespar;

import java.io.*;
import com.aspden.tespar.statstrategies.*;
import com.aspden.tespar.basics.*;
import com.aspden.graph.*;
import java.awt.*;
import java.awt.event.*;

public class DemoSoundClassifier {
    private SoundWaveTable wavetable;
    private boolean fileclassifieractive=false;
    private String[] words;
    private String[] filenames;
    
    private static final int FREQUENCY=11025;

    private class ClassificationHelper implements ClassifierPanel.ClassifierListener
    {
        SoundWave wave;
        Frame f;
        File theFile;
        
        public ClassificationHelper(File theFile) throws IOException
        {
            fileclassifieractive=true;
            
            this.theFile=theFile;
            this.wave =new SoundWave(theFile, FREQUENCY);
            
            ClassifierPanel a=new ClassifierPanel(wave.getDisplay(), words);
            a.addListener(this);
            
            f=new SingleComponentFrame(a,theFile.getPath());
            wave.play();
        }
        public void classified(int i){
            System.out.println("classified as "+words[i]);
            wavetable.add(wave, filenames[i]);
             f.dispose();
            fileclassifieractive=false;
        }
        public void unclassifiable(){
            System.out.println("unclassifiable");
            f.dispose();
            fileclassifieractive=false;
        }
    }

    public DemoSoundClassifier(File database, File newsounds, String[] words, String[] filenames) throws IOException
    {
        this.wavetable=new SoundWaveTable(database);
        this.words=words;
        this.filenames=filenames;

        //new SingleComponentFrame(wavetable.getDisplay(),database.getCanonicalPath(),0.5,0.75);

        File[] theFiles=newsounds.listFiles();
        for(int i=0; i<theFiles.length; i++)
        {
            new ClassificationHelper(theFiles[i]);
            while(fileclassifieractive) com.aspden.Stuff.threadSleep(100);
            System.out.println("delete"+theFiles[i].delete());
        }
        System.exit(0);
    }

    /**
     * @param args ignored
     */
    public static void main (String args[]) throws IOException {
        final SpeechDatabase sdb=SpeechDatabase.JohnDigitsSony;
        LookAndFeelStuff.randomise();
        new DemoSoundClassifier(new File("E:\\livestuff"), new File("E:\\demosounds"), sdb.getWords(), sdb.getFiles());

    }
}


