/*
 * SoundGrabPanel.java
 */
package com.aspden.soundrecorder;

import com.aspden.graph.*;

import javax.sound.sampled.*;
import java.awt.*;
import javax.swing.*;

/** Captures sound and displays both the captured wave-form and a moving average energy level in real time.
 */
public class AnimatedSoundCapturer extends Object {

    private SoundCatcher theSoundCatcher;
    private MovingWaveDisplayer theMovingWaveDisplayer;
    private int frequency;
    private int seconds;
    private int lastsize;
    private double energy;
    private int energyWindowSize;


    /** Create an AnimatedSoundCapturer.
     * Set the length, frequency and energy trace calculation parameters.
     * @param frequency sample frequency in Hz.
     * @param seconds length of sound to capture in seconds.
     * @param energyWindowSize length of moving average energy window (in samples)
     * @param energyscale scaling factor to display energy trace.
     * @throws LineUnavailableException If the system cannot provide a sound input line.
     */
    public AnimatedSoundCapturer(int frequency,int seconds,int energyWindowSize,double energyscale) throws LineUnavailableException
    {
        this.frequency=frequency;
        this.seconds=seconds;
        this.energyWindowSize=energyWindowSize;
        theMovingWaveDisplayer = getDisplay(frequency, seconds, energyscale);
    }

    /**Creates a plot with a blue and a red line, where the blue scale is -32768,32767 and the red is from -frequency, frequency*/
    private MovingWaveDisplayer getDisplay(double frequency, double seconds, double energyscale)
    {
return new MovingWaveDisplayer((int)(seconds*frequency),2, new double[]{Short.MIN_VALUE,0}, new double[]{Short.MAX_VALUE,energyscale}, new Color[]{Color.blue, Color.red});
    }

    /** Get the animated graphical component.
     * @return an animated display of incoming sound and energy levels.
     */
    public MovingWaveDisplayer getPanel() {
        return theMovingWaveDisplayer;
    }

    /** Start capturing sound and running the display.
     * @throws LineUnavailableException If the system cannot provide a sound input.
     * @return the captured sound samples.
     */
    public double[] getSound() throws LineUnavailableException {
        lastsize=0;
        energy=0;

        theMovingWaveDisplayer.reset();
        theSoundCatcher=new SoundCatcher(frequency, seconds);
         theSoundCatcher.start();
 

        while(theSoundCatcher.isAlive())
        {
            display();
            com.aspden.Stuff.threadSleep(10);
        }
        display();
        
        int[] buf=theSoundCatcher.getBuffer();
        int len=theSoundCatcher.getWritePosition();
        double[] d=new double[len];
        for(int i=0; i<len; i++) d[i]=buf[i];

        theSoundCatcher=null;
        return d;
    }

    private void display()
    {
        int size=theSoundCatcher.getWritePosition();
        int[] buf=theSoundCatcher.getBuffer();

        for(int i=lastsize; i<size; i++)
        {
            theMovingWaveDisplayer.add(0,buf[i]);
            energy+=(buf[i]*buf[i]);
            if(i>=energyWindowSize) energy-=(buf[i-energyWindowSize]*buf[i-energyWindowSize]);
            theMovingWaveDisplayer.add(1,Math.sqrt(energy/energyWindowSize));
        }
        lastsize=size;

    }


    /** Demo/Test Code
     * @param args Command Line Arguments. Ignored.
     * @throws LineUnavailableException If the system cannot provide a sound input.
     */
    public static void main (String args[]) throws LineUnavailableException {
        int FREQUENCY=11025;
        AnimatedSoundCapturer theSoundGrabPanel=new AnimatedSoundCapturer(FREQUENCY, 10, 1000, 100);
        JFrame f=new JFrame("Animated Sound Capturer Demo");
        f.setBackground(Color.white);
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        f.getContentPane().add(theSoundGrabPanel.getPanel());
        f.pack();
        f.show();

        for(int i=0; i<1; i++)
        {
            double[] sound = theSoundGrabPanel.getSound();
            (new com.aspden.soundplay.Player(sound, FREQUENCY)).play();
            new SingleComponentFrame(new ArrayDisplayPanel(sound),"Sound Captured by Animated Sound Capturer");
        }
    }

}

