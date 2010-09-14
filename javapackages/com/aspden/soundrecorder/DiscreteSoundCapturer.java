/*
 * DiscreteSoundCapturer.java
 */

package com.aspden.soundrecorder;

import javax.sound.sampled.*;
import javax.swing.*;
import java.awt.*;
import com.aspden.graph.*;
import java.util.*;

/** Captures and displays in real time a few seconds of sound input
 * and splits it into discrete words (or regions of high energy).
 */
public class DiscreteSoundCapturer extends Object {

    private AnimatedSoundCapturer theCapturer;
    private int energyWindowInSamples;
    private double energyThreshold;


    /** Create a DiscreteSoundCapturer.
     * On creation, a frame in which the sounds will be displayed appears.
     * Captured sound will appear in this frame in real time,
     * together with an energy level line which will rise off the screen
     * when the volume becomes loud enough that the sound is considered to be a word.
     * @param frequency Frequency to sample the incoming sound.
     * @param seconds No of seconds of sound to capture.
     * @param energyThreshold Energy at which sound is to be considered a 'word'.
     * @param energyWindow length of energy window over which to take average when determining presence of speech.
     * @throws LineUnavailableException If the system can't provide a sound input of the required type.
     */
    public DiscreteSoundCapturer(int frequency,int seconds,double energyThreshold,double energyWindow) throws LineUnavailableException {
        this.energyWindowInSamples=(int)(frequency*energyWindow);
        this.energyThreshold=energyThreshold;
        theCapturer=new AnimatedSoundCapturer(frequency,seconds, energyWindowInSamples, energyThreshold);

        JFrame f=new JFrame("Live signal Capture");
        f.setBackground(Color.white);
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        f.getContentPane().add(theCapturer.getPanel());
        Dimension d=Toolkit.getDefaultToolkit().getScreenSize();
        f.setSize((int)d.width,(int)(d.height/6));
        f.show();
    }

    /** Record originally specified amount of speech, and split into
     * discrete words according to originally prescribed energy levels.
     * @throws LineUnavailableException If the system cannot provide a sound input of the specified type.
     * @return A vector of doubles each member of which represents a distinct passage of high energy sound.
     */
    public Vector getSounds() throws LineUnavailableException
    {
        double[] sound = theCapturer.getSound();
        return slice(sound, energyThreshold, energyWindowInSamples );
    }

    private Vector getSlices(double[] sound, Vector changePoints)
    {
        Vector slices=new Vector();
        Iterator i=changePoints.iterator();

        int a=0;
        boolean speech=false;
        while(i.hasNext())
        {
            int b=((Integer)(i.next())).intValue();
            if (speech)
            {
                if((b-a)>(4*energyWindowInSamples/3)) //Intervals not significantly longer than the window length are unlikely to be speech.
                {
                    double[] s=new double[b-a+1];
                    for(int j=0; j<s.length;j++)
                    {
                        s[j]= sound[j+a];
                    }
                    slices.add(s);
                }
            }
            a=b;
            speech=!speech;
        }
        return slices;
    }

    private Vector slice(double[] sound, double threshold, int windowLength)
    {
        double[] energy = calculateEnergy(sound, windowLength);

        Vector changePoints = getThresholdPoints(energy, threshold);

        return getSlices(sound, changePoints);

    }


    private double[] getDouble(int[] a)
    {
        double[] d= new double[a.length];
        for(int i=0; i<a.length; i++) d[i]=a[i];
        return d;
    }

    private double[] calculateEnergy(double[] signal, int windowsize)
    {
        double[] energy = new double[signal.length];

        int halfwindow=windowsize/2;

        double ensquared=0;
        for(int i=0; i<windowsize; i++) ensquared+=signal[i]*signal[i];
        for(int i=0; i<halfwindow; i++) energy[i]=Math.sqrt(ensquared/windowsize);
        for(int i=halfwindow; i<signal.length-halfwindow; i++)
        {
            ensquared+=signal[i+halfwindow]*signal[i+halfwindow];
            ensquared-=signal[i-halfwindow]*signal[i-halfwindow];
            energy[i]=Math.sqrt(ensquared/windowsize);
        }
        for(int i=signal.length-halfwindow; i<signal.length; i++) energy[i]=Math.sqrt(ensquared/windowsize);

        /*double[] wave=new double[signal.length];
        for(int i=0; i<signal.length; i++) wave[i]=signal[i];
        new SingleComponentFrame(new ArrayDisplayPanel(wave, energy),"Energy calculation", 1.0, 0.2);*/

        return energy;
    }

    private Vector getThresholdPoints(double[] energy, double threshold)
    {
        Vector changePoints=new Vector();

        int i=0;

        //Throw away any noise at the start.
        while(i<energy.length && energy[i]>threshold) i++;

        boolean last=false;
        //Now record every change from noisy to not-noisy
        for(; i<energy.length; i++)
        {
            boolean energetic=(energy[i]>threshold);
            if(energetic!=last){
                changePoints.add(new Integer(i));
            }
            last=energetic;
        }
        return changePoints;
    }


    /** Test/Demo code. Record some speech and display the returned discrete waves.
     * @param args Command line arguments (ignored)
     * @throws LineUnavailableException If system cannot supply sound input.
     */
    public static void main (String args[]) throws LineUnavailableException
    {
        int frequency=16000;
        //Create a 20 second discrete-word capturer with 16kHz sampling, energy threshold of 100, with a 1/3 second energy averaging window.
        DiscreteSoundCapturer g = new DiscreteSoundCapturer(frequency, 3, 100, 1.0/3.0);

        for(;;)
        {
            //Record some sounds
            Vector sounds=g.getSounds();

            //Display the sounds
            int w=0;
            for(Iterator i= sounds.iterator();i.hasNext();)
            {
                double[] s = (double[])i.next();
                new SingleComponentFrame(new ArrayDisplayPanel(s),"word "+w++);
                (new com.aspden.soundplay.Player(s, frequency)).play();
            }
        }
    }


}