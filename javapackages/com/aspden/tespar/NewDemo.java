/*
 * NewDemo.java
 */

package com.aspden.tespar;

import com.aspden.tespar.statstrategies.*;
import com.aspden.tespar.statscomparison.*;
import com.aspden.tespar.statistics.*;
import com.aspden.tespar.basics.*;
import com.aspden.tespar.utility.*;
import com.aspden.soundrecorder.*;
import com.aspden.graph.*;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.io.*;

/** A big collection of calculated statistics against which single statistics can be compared.
 */
public class NewDemo extends Object implements SoundListener{
    private TextArea theJTextArea=new TextArea();
    private int wavecount=0;
    
    private int frequency;

    private static boolean recordButtonPressed;
    private static final JButton recordButton=new JButton("record");
    private static final String demoRecordDir="C:\\demosounds";
    
    private Corpus theCorpus;


    public NewDemo(StatisticAcquisitionStrategy theStrategy, boolean detailedDisplays, int frequency) {
        theJTextArea.setFont(new Font("SansSerif",Font.PLAIN, 24));
        Frame f=new SingleComponentFrame(theJTextArea,"Results");
        f.show();
        
        theCorpus=new Corpus(theStrategy, detailedDisplays);
        this.frequency=frequency;
    }

    public void display(final String s)
    {
        SwingUtilities.invokeLater(new Runnable(){
            public void run(){
                theJTextArea.append(s);
            }
        });
    }

    public void createWordsWindow(String[] theWords)
    {
        JFrame f=new JFrame("Candidate Words");
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        JTextArea a=new JTextArea();
        f.getContentPane().add(a);
        for(int i=0; i<theWords.length; i++) a.append(theWords[i]+"\n");
        f.pack();
        f.show();
    }

    public void add(SpeechDatabase s) throws IOException
    {
        theCorpus.add(s);
        createWordsWindow(s.getWords());
    }

    public void soundRecorded(double[] sound)
    {
        SoundWave s=new SoundWave(sound, frequency);
        
        new File(demoRecordDir).mkdir();
        System.out.println("starting write "+wavecount);
        s.writeToFile(new File(demoRecordDir+File.separator+"wave"+wavecount+".txt"));
        System.out.println("starting compare "+wavecount);
        
        compare(s, "wave "+wavecount);
        
        wavecount++;
    }

    public void compare(SoundWave soundwave, String name)
    {
        String answer=theCorpus.compare(soundwave, name);
        display(answer+" ");
    }


    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) throws Exception {

        int FREQUENCY=11025;
        boolean DETAILS=false;

        recordButton.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent e){
                recordButtonPressed=!recordButtonPressed;
                if(recordButtonPressed) recordButton.setText("recording");
                else recordButton.setText("stopping");
            }
        });

        NewDemo theDemo = new NewDemo(new DefaultStrategy(), DETAILS, FREQUENCY);
        theDemo.add(SpeechDatabase.MoreOguzDigitsSony11k);

        AnimatedSoundDispatcher theSoundGrabPanel=new AnimatedSoundDispatcher(FREQUENCY,20, 1.0/3.0, 100);
        theSoundGrabPanel.addSoundListener(theDemo);


        JFrame f=new JFrame("Combined Demo");
        f.setBackground(Color.white);
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        f.getContentPane().setLayout(new BorderLayout());
        f.getContentPane().add(theSoundGrabPanel.getPanel(),BorderLayout.CENTER);
        f.getContentPane().add(recordButton, BorderLayout.WEST);
        f.pack();
        f.show();

        for(;;)
        {
            if(!recordButtonPressed) recordButton.setText("record");
            while(!recordButtonPressed) com.aspden.Stuff.threadSleep(1);
            theSoundGrabPanel.getSound();
            theDemo.display("\n");
        }
    }

}
