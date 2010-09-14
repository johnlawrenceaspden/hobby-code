/*
 * AnimatedSoundDespatcher.java
 */
package com.aspden.soundrecorder;

import com.aspden.graph.*;

import javax.sound.sampled.*;
import java.awt.*;
import javax.swing.*;
import java.util.*;

/** An animated sound / energy display which sends recordings of periods of high energy to attached {@link SoundListener}s as soon as they happen.
 */
public class AnimatedSoundDispatcher extends Object {

    private SoundCatcher theSoundCatcher;
    private MovingWaveDisplayer theMovingWaveDisplayer;
    private int frequency;
    private int seconds;
    private int lastsize;
    private double instantaneousSquareEnergy;
    private int energyWindowSize;
    private double energyScale;

    private double[] samples;
    private double[] energy;
    private int bufferPosition;
    private int lastLowEnergy;

    private Vector theSoundListeners=new Vector();


    /** Create an AnimatedSoundDespatcher.
     * Set the length of time for which the capture is to go on,
     * the sampling frequency and the energy parameters which determine what
     * counts as a sound and gets fired off to attached SoundListeners.
     * @param frequency sample frequency in Hz.
     * @param seconds length of sound to capture in seconds.
     * @param energyscale Both the energy level which determines when a sound starts and finishes, and the scale for the energy trace display, so that the red energy line goes off the scale just as the sound capture is activated. <I>i.e.</I> you only ever see the energy of the silence.
     * @param energyWindow size of the energy calculation window in seconds.
     * @throws LineUnavailableException If the system cannot provide a sound input line.
     */
    public AnimatedSoundDispatcher(int frequency,int seconds,double energyWindow,double energyscale) throws LineUnavailableException
    {
        this.frequency=frequency;
        this.seconds=seconds;
        this.energyWindowSize=(int)(energyWindow*frequency);
        this.energyScale=energyscale;
        theMovingWaveDisplayer = getDisplay(frequency, seconds, energyscale);
        this.samples = new double[frequency*seconds];
        this.energy = new double[frequency*seconds];
    }


    /** Start capturing sound, running the display and firing off high energy events to the listeners.
     * @throws LineUnavailableException If the system cannot provide a sound input.
     * @return All the sound captured during the run. Not divided into chunks like the broadcast events.
     */
    public double[] getSound() throws LineUnavailableException {
        lastsize=0;
        instantaneousSquareEnergy=0;
        bufferPosition=0;

        theMovingWaveDisplayer.reset();
        theSoundCatcher=new SoundCatcher(frequency, seconds);
        theSoundCatcher.start();


        while(theSoundCatcher.isAlive())
        {
            processSound();
            com.aspden.Stuff.threadSleep(10);
        }
        processSound();

        double[] d=new double[bufferPosition];
        for(int i=0; i<bufferPosition; i++) d[i]=samples[i];

        theSoundCatcher=null;
        return d;
    }

    /**If there are any new samples in the SoundCatcher's buffer then we want to calculate energy values for them and transfer both to the display.
    If we notice any reasonably lengthy high energy periods then we should broadcast them to the objects which are listening in.*/
    private void processSound()
    {
        int size=theSoundCatcher.getWritePosition();
        int[] buf=theSoundCatcher.getBuffer();

        if(size>this.samples.length)
        {
            System.out.println("Internal Error Warning! SoundCatcher has produced more samples than expected by AnimatedSoundDespatcher. Truncating...");
            System.out.println("Size"+size+"length"+this.samples.length);
            size=this.samples.length-1; //SoundCatcher should not produce more samples, but just in case.
        }

        for(int i=lastsize; i<size; i++)
        {
            int sample=buf[i];
            instantaneousSquareEnergy+=(sample*sample);
            if(i>=energyWindowSize) instantaneousSquareEnergy-=(buf[i-energyWindowSize]*buf[i-energyWindowSize]);
            double instantaneousEnergy=Math.sqrt(instantaneousSquareEnergy/energyWindowSize);

            theMovingWaveDisplayer.add(0,sample);
            theMovingWaveDisplayer.add(1,instantaneousEnergy);

            this.samples[bufferPosition]=sample;
            this.energy[bufferPosition]=instantaneousEnergy;
            if(instantaneousEnergy<energyScale) //If we're in low energy
            {
                //check whether we've just been in a long period of high energy.
                if(lastLowEnergy+(4*energyWindowSize/3)<bufferPosition) //An event must be 4/3 of the energy window size to count. (A quick click can influence a whole energy window.)
                {
                    int start=lastLowEnergy-energyWindowSize;    //energy is a lagging indicator so send back extra samples from the start.
                    if(start<0) start=0;
                    broadcastSound(start, bufferPosition);
                }
                lastLowEnergy=bufferPosition;
            }
            bufferPosition++;
        }
        lastsize=size;

    }

    /**Creates a plot with a blue and a red line, where the blue scale is -32768,32767 and the red is from 0 to energyscale(So that the energy plot goes off the screen at the point where we consider it loud enough to be speech.)*/
    private MovingWaveDisplayer getDisplay(double frequency, double seconds, double energyscale)
    {
        return new MovingWaveDisplayer(
        (int)(seconds*frequency),2,
        new double[]{
        Short.MIN_VALUE,0},
        new double[]{
        Short.MAX_VALUE,energyscale},
        new Color[]{
        Color.blue, Color.red}
        );
    }

    /** Standard Listener type interface.
     * @param theSoundListener An object which wishes to be told about high energy events.
     */
    public void addSoundListener(SoundListener theSoundListener)
    {
        theSoundListeners.add(theSoundListener);
    }

    /**Send the sound between a and b out to any attached listeners.*/
    private void broadcastSound(int a, int b)
    {
        double[] sound=new double[b-a+1];
        for(int i=a; i<=b; i++) sound[i-a]=samples[i];
        Iterator i=theSoundListeners.iterator();
        while(i.hasNext())
        {
            ((SoundListener)i.next()).soundRecorded(sound);
        }
    }


    /** Get the animated graphical component.
     * @return an animated display of incoming sound and energy levels.
     */
    public MovingWaveDisplayer getPanel() {
        return theMovingWaveDisplayer;
    }


    /** Demo/Test Code
     * @param args Command Line Arguments. Ignored.
     * @throws LineUnavailableException If the system cannot provide a sound input.
     */
    public static void main (String args[]) throws LineUnavailableException {

        final int FREQUENCY=11025;

        SoundListener displayer = new SoundListener(){
            public void soundRecorded(double[] sound){
                new SingleComponentFrame((new com.aspden.tespar.basics.SoundWave(sound, FREQUENCY).getDisplay()),"Sound Captured by Animated Sound Despatcher");
            }
        };


        AnimatedSoundDispatcher theSoundGrabPanel=new AnimatedSoundDispatcher(FREQUENCY,20, 1.0/3.0, 100);
        theSoundGrabPanel.addSoundListener(displayer);

        JFrame f=new JFrame("Animated Sound Despatcher Demo");
        f.setBackground(Color.white);
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        f.getContentPane().add(theSoundGrabPanel.getPanel());
        f.pack();
        f.show();

        for(int i=0; i<1; i++)
        {
            double[] sound = theSoundGrabPanel.getSound();
            //(new com.aspden.soundplay.Player(sound, FREQUENCY)).play();
            //new SingleComponentFrame(new ArrayDisplayPanel(sound),"Sound Captured by Animated Sound Despatcher");
        }
    }

}

