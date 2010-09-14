/*
 * SoundRecorder.java
 */

package com.aspden.soundrecorder;

import com.aspden.graph.*;
import com.aspden.graphwidget.misc.*;


import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.util.*;
import javax.sound.sampled.*;
import java.io.*;
import com.aspden.soundplay.*;
import com.aspden.tespar.basics.*;


/** Application to record discrete speech samples */
public class SoundRecorder implements ActionListener {
    //private final String[] words={"o","z","1","2","3","4","5","6","7","8","9"};
    //private final String[] words={"nextlevel","messages","callregister","profile","settings","calldivert","games","calculator","infrared","calendar"};
    private final String[] directories={"record00", "record01", "record02", "record03", "record04", "record05", "record06",
        "record07", "record08", "record09", "record10", "record11", "record12","record13", "record14", "record15",
    "record16","record17", "record18", "record19","record20", "record21", "record22", "record23","record24", "record25"};
    private final String rootdirectory="C:\\new speech database";

    private final String[] words={"Alpha", "Bravo", "Charlie", "Delta", "Echo", "Foxtrot", "Golf", "Hotel", "India", "Juliet", "Kilo", "Lima", "Mike", "November", "Oscar", "Papa", "Quebec", "Romeo", "Sierra", "Tango", "Uniform", "Victor", "Whiskey", "Xray", "Yankee", "Zulu"};

    private int frequency;


    private int directory=0;
    JFrame theButtonFrame;
    JFrame theSamplesFrame;
    JButton recordButton = new JButton("record");
    JButton playButton = new JButton("play");
    JButton writeButton = new JButton("write");
    boolean recordButtonDown=false;
    boolean playButtonDown=false;
    boolean writeButtonDown=false;
    DiscreteSoundCapturer theCapturer;
    Vector theSounds;

    /** Creates new SoundRecorder */
    public SoundRecorder(int frequency) throws LineUnavailableException, IOException{
        
        this.frequency=frequency;
      
        recordButton.addActionListener(this);
        writeButton.addActionListener(this);
        playButton.addActionListener(this);

        theButtonFrame=new JFrame("Sound Recorder");
        theButtonFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        theButtonFrame.getContentPane().setLayout(new GridLayout(0,1));
        theButtonFrame.getContentPane().add(recordButton);
        theButtonFrame.getContentPane().add(playButton);
        theButtonFrame.getContentPane().add(writeButton);
        theButtonFrame.pack();
        theButtonFrame.show();

        theSamplesFrame=new JFrame("Recorded Samples");
        theSamplesFrame.getContentPane().setLayout(new GridLayout());

        Dimension d=Toolkit.getDefaultToolkit().getScreenSize();

        theSamplesFrame.setSize(d.width,d.height/10);
        theSamplesFrame.show();

        theCapturer=new DiscreteSoundCapturer(frequency, words.length*8/5, 100, 1.0/3.0);

        JFrame theCrib=new JFrame();
        JTextArea theCribArea=new JTextArea();
        for(int i=0; i<words.length; i++)theCribArea.append(words[i]+"\n");
        theCrib.getContentPane().add(theCribArea);
        theCrib.pack();
        theCrib.show();

        File f=new File(rootdirectory);
        f.delete();

        //wait for record button
        buttonWait(true,false,false);

        for(;;)
        {
            recordAndDisplay();
            for(;;)
            {
                buttonWait(true, true, true);
                if(playButtonDown)
                {
                    play(theSounds);
                }
                else if (writeButtonDown)
                {
                    writeToDisk(theSounds, words);
                    buttonWait(true,false,false);
                    break;
                }
                else if(recordButtonDown) break;
            }
        }
    }

    private void recordAndDisplay() throws LineUnavailableException
    {
        theSounds = theCapturer.getSounds();

        Runnable updateAComponent = new Runnable() {
            public void run() {
                display(theSounds);
            }
        };
        SwingUtilities.invokeLater(updateAComponent);
    }

    private void buttonWait(boolean recordEnabled, boolean playEnabled, boolean writeEnabled)
    {
        recordButton.setEnabled(recordEnabled);
        writeButton.setEnabled(playEnabled);
        playButton.setEnabled(writeEnabled);
        recordButtonDown=false;
        writeButtonDown=false;
        playButtonDown=false;

        while(recordButtonDown==false && playButtonDown==false && writeButtonDown==false)
        try {
            Thread.sleep(1);
        }catch(InterruptedException e){
        };
    }

    private void play(Vector theSounds)
    {
        Iterator it= theSounds.iterator();
        while(it.hasNext())
        {
            double[] s=(double[])it.next();
            try{
                (new com.aspden.soundplay.Player(s, frequency)).play();
            }catch(LineUnavailableException e){
                System.out.println("Line Unavailable");
                System.out.println(e);
            }

        }
    }

    private void writeToDisk(Vector sounds, String[] words) throws IOException
    {
        String thisdir=rootdirectory;
        File f=new File(thisdir);
        f.mkdir();
        thisdir=thisdir+File.separator+directories[directory];
        f=new File(thisdir);
        f.mkdir();

        Iterator it= sounds.iterator();
        int i=0;
        while(it.hasNext() && i<words.length)
        {
            double[] s=(double[])it.next();
            writeFile(s,thisdir+File.separator+words[i++]+".txt");
        }
        directory++;
    }

    private void writeFile(double[] s, String name) throws IOException
    {
            PrintWriter p=new PrintWriter(new FileWriter(name));
            for(int i=0; i<s.length; i++) p.println((int)s[i]);
            p.close();
    }

    private void display(Vector theSounds)
    {
        Iterator it= theSounds.iterator();
        int i=0;
        theSamplesFrame.getContentPane().removeAll();

        Label noLabel=new Label(theSounds.size()+" recorded");
        if(theSounds.size()!=words.length) noLabel.setBackground(Color.red);
        theSamplesFrame.getContentPane().add(noLabel);
        while(it.hasNext())
        {
            double[] s=(double[])it.next();
            java.awt.Component p=new SoundWave(s, frequency).getDisplay();
            theSamplesFrame.getContentPane().add(p);
        }
        theSamplesFrame.validate();
    }

    private Vector recordSounds() throws LineUnavailableException
    {
        Vector theSounds=new Vector();
        theSounds=theCapturer.getSounds();
        return theSounds;

    }

    public void actionPerformed(final java.awt.event.ActionEvent e) {
        if(e.getSource()==recordButton)
        {
            recordButtonDown=true;
        }
        else if(e.getSource()==playButton)
        {
            playButtonDown=true;
        }
        else if(e.getSource()==writeButton)
        {
            writeButtonDown=true;
        }
    }

    /**Runs the sound catching application*/
    public static void main (String args[]) throws LineUnavailableException, IOException {
        LookAndFeelStuff.randomise();
        new SoundRecorder(16000);

    }
}