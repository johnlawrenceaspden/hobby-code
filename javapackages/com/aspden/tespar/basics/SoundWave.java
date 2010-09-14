/*
 * SoundWave.java
 */

package com.aspden.tespar.basics;

import java.io.*;
import java.util.*;
import java.awt.*;
import javax.sound.sampled.*;
import com.aspden.graph.*;
import com.aspden.tespar.utility.*;

/** A representation of a sound wave */
public class SoundWave extends Object implements RightClickListener{

    public double[] data;
    private int frequency;

    /** Creates new SoundWave given a file of numbers in newline separated ASCII format and a sampling frequency.
     * @param soundFile A newline separated ASCII file of numbers representing a soundwave
     */
    public SoundWave(File textFile, int frequency) throws IOException {
        this.frequency=frequency;
        Vector dataVector = new Vector();
        StreamTokenizer strtok=new StreamTokenizer(new BufferedReader(new FileReader(textFile)));
        strtok.nextToken();
        while(strtok.ttype!=strtok.TT_EOF){
            dataVector.add(new Double(strtok.nval));
            strtok.nextToken();
        }
        data=new double[dataVector.size()];
        int j=0;
        for(Iterator i=dataVector.iterator(); i.hasNext();) data[j++]=((Double)i.next()).doubleValue();
    }



    public SoundWave(File soundFile) throws IOException {
        try{
            AudioInputStream audioInputStream = AudioSystem.getAudioInputStream(soundFile);
            AudioFormat audioFormat=audioInputStream.getFormat();
            this.frequency=(int)audioFormat.getSampleRate();

            if(audioFormat.getEncoding()!=AudioFormat.Encoding.PCM_SIGNED && audioFormat.getEncoding()!=AudioFormat.Encoding.PCM_UNSIGNED) throw new UnsupportedAudioFileException("Can't deal with non-PCM encodings");
            byte[] audioBytes = new byte[(int)(audioInputStream.getFrameLength()*audioFormat.getFrameSize())];
            audioInputStream.read(audioBytes);

            if (audioFormat.getSampleSizeInBits() == 16) {
                int nlengthInSamples = audioBytes.length / 2;
                data = new double[nlengthInSamples];
                if (audioFormat.isBigEndian()) {
                    for (int i = 0; i < nlengthInSamples; i++) {
                        /* First byte is MSB (high order) */
                        int MSB = (int) audioBytes[2*i];
                        /* Second byte is LSB (low order) */
                        int LSB = (int) audioBytes[2*i+1];
                        data[i] = MSB << 8 | (255 & LSB);
                    }
                } else {
                    for (int i = 0; i < nlengthInSamples; i++) {
                        /* First byte is LSB (low order) */
                        int LSB = (int) audioBytes[2*i];
                        /* Second byte is MSB (high order) */
                        int MSB = (int) audioBytes[2*i+1];
                        data[i] = MSB << 8 | (255 & LSB);
                    }
                }
            } else if (audioFormat.getSampleSizeInBits() == 8) {
                int nlengthInSamples = audioBytes.length;
                data = new double[nlengthInSamples];
                if (audioFormat.getEncoding().toString().startsWith("PCM_SIGN")) {
                    for (int i = 0; i < audioBytes.length; i++) {
                        data[i] = audioBytes[i];
                    }
                } else {
                    for (int i = 0; i < audioBytes.length; i++) {
                        data[i] = audioBytes[i] - 128;
                    }
                }
            }
        }catch(UnsupportedAudioFileException e){
            throw new IOException("Unknown Audio File type (Originally UnsupportedAudioFileException)"+e);
        }

    }




    /** Creates a new SoundWave from a set of samples at a given frequency.
     * @param samples 16-bit 16kHz samples as doubles.
     */
    public SoundWave(double[] samples, int frequency) {
        this.data=(double[])samples.clone();
        this.frequency=frequency;
    }

    /** Endpoint (snip off the silence surrounding the high energy region) using the energy level at the beginning of the wave as the silence energy.
     * @param windowsize Number of samples to use when taking the moving average used to calculate the energy.
     * @param threshold Ratio by which the energy has to increase over the silence level before the sound is considered significant.
     * @return the filtered SoundWave.
     */
    public SoundWave endpointRelative(int windowsize, double threshold) {
        double[] energy = Mathematics.calculateH1Energy(windowsize, this.data);
        double[] sortedenergy=Mathematics.sorted(energy);
        int begin = Mathematics.findFirstPointAbove(threshold+1.1*sortedenergy[data.length/100], energy);
        int end = Mathematics.findLastPointAbove(threshold+1.1*sortedenergy[data.length/100], energy);
        return new SoundWave(Mathematics.chop(begin, end, data), this.frequency);
    }

    /** Endpoint (snip off the silence surrounding the high energy region)
     * using a given threshold.
     * @param windowsize Number of samples to use when taking the moving average used to calculate the energy.
     * @param threshold Minimum energy level for the sound to be considered significant.
     * @return the filtered SoundWave.
     */
    public SoundWave endpointAbsolute(int windowsize, double threshold)
    {
        double[] energy = Mathematics.calculateH1Energy(windowsize, this.data);
        int begin = Mathematics.findFirstPointAbove(threshold, energy);
        int end =  Mathematics.findLastPointAbove(threshold, energy);
        return new SoundWave(Mathematics.chop(begin, end, data), this.frequency);
    }

    
    double[] getData()
    {
        return (double[]) data.clone();
    }

    /** Play the sound.
     */
    public void play()
    {      try{
            (new com.aspden.soundplay.Player(this.data, this.frequency)).playLoud();
        }catch(javax.sound.sampled.LineUnavailableException e){
            System.out.println("Can't play sound. Line Unavailable");
            System.out.println(e);
        }
    }


    /** Get a visual representation of the sound wave.
     * @return A component which contains a graph of the sound wave. A right-click on this component will cause the SoundWave to play.
     */
    public Component getDisplay() {
        ArrayDisplayPanel a = new ArrayDisplayPanel(this.data, -10000, 10000);
        a.addRightClickListener(this);
        return a;
    }

    /** Causes the wave to play. Called by the component returned by {@link #getDisplay()}
     */
    public void rightClick() {
        play();
    }

    /** Get a visual representation of the sound wave and the associated energy calculated using a given window size.
     * @return A component which contains a graph of the sound wave and the energy level.
     * @param windowsize no of samples to be used for moving average window when calculating energy
     */
    public Component getEnergyDisplay(int windowsize)
    {
        double[] energy = Mathematics.calculateH1Energy(windowsize, this.data);
        ArrayDisplayPanel a = new ArrayDisplayPanel(this.data,-10000,10000,energy,-1000,1000);
        a.addRightClickListener(this);
        return a;
    }

    public void writeToFile(final File f)
    {
        Runnable writer = new Runnable() {
            public void run()  {
                try{
                    PrintWriter p=new PrintWriter(new FileWriter(f));
                    for(int i=0; i<data.length; i++) p.println((int)data[i]);
                    p.close();
                }catch(IOException e){
                    System.out.println("Unable to write!!!"+e);
                }
            }
        };
        new Thread(writer).start();
    }


//    /**
//     * Removes dc from the wave by working out the average and subtracting it from all points.
//     * @return the filtered SoundWave.
//     */
//    public SoundWave removeDC()
//    {
//        double average=0.0;
//        for(int i=0; i<data.length-0; i++) average+= data[i];
//        average/=data.length;
//
//        double[] filtered=new double[data.length];
//        for(int i=0; i<data.length-0; i++) filtered[i]-=average;
//        return new SoundWave(filtered, this.frequency);
//    }


    /** A Butterworth order 1 filter designed to band pass 120-4300Hz on a sample rate of
     * 16000Hz. See Tony Fisher's web page at York University for derivation
     * @return the filtered SoundWave.
     */
    public SoundWave filterButterworthBandPassOrder1()
    {
        double[] filtered;
        if(frequency==16000)
        {
            filtered=DigitalFilter.ButterworthOrder1_16000Hz_BandPass_120_4300Hz(this.data);
        }
        else if (frequency==11025)
        {
            filtered=DigitalFilter.ButterworthOrder1_11025Hz_BandPass_120_4300Hz(this.data);
        }
        else throw new IllegalStateException("Don't have a filter for this frequency.");

        return new SoundWave(filtered, frequency);
    }

    /**
     * A Butterworth order 8 filter designed to high pass above 300Hz on a sample rate of
     * 16000Hz. See Tony Fisher's web page at York University for derivation
     * @return the filtered SoundWave.
     */
    public SoundWave filterButterworthHighPassOrder8()
    {
        double[] filtered;
        if(frequency==16000)
        {
            filtered=DigitalFilter.ButterworthOrder8_16000Hz_HighPass_300Hz(this.data);
        }
        else if (frequency==11025)
        {
            filtered=DigitalFilter.ButterworthOrder8_11025Hz_HighPass_300Hz(this.data);
        }
        else throw new IllegalStateException("Don't have a filter for this frequency.");

        return new SoundWave(filtered, frequency);
    }



    /**
     * A Butterworth order 8 filter designed to low pass below 4300Hz on a sample rate of
     * 16000Hz. See Tony Fisher's web page at York University for derivation
     * @return the filtered SoundWave.
     */
    public SoundWave filterButterworthLowPassOrder8()
    {
        double[] filtered;
        if(frequency==16000)
        {
            filtered=DigitalFilter.ButterworthOrder8_16000Hz_LowPass_4300Hz(this.data);
        }
        else if (frequency==11025)
        {
            filtered=DigitalFilter.ButterworthOrder8_11025Hz_LowPass_4300Hz(this.data);
        }
        else throw new IllegalStateException("Don't have a filter for this frequency.");

        return new SoundWave(filtered, frequency);
    }


    /**
     * A Butterworth order 2 filter designed to low pass below 4300Hz on a sample rate of
     * 16000Hz. See Tony Fisher's web page at York University for derivation
     * @return the filtered SoundWave.
     */
    public SoundWave filterButterworthLowPassOrder2()
    {
        double[] filtered;
        if(frequency==16000)
        {
            filtered=DigitalFilter.ButterworthOrder2_16000Hz_LowPass_4300Hz(this.data);
        }
        else if (frequency==11025)
        {
            filtered=DigitalFilter.ButterworthOrder2_11025Hz_LowPass_4300Hz(this.data);
        }
        else throw new IllegalStateException("Don't have a filter for this frequency.");
        return new SoundWave(filtered, frequency);
    }
    
    /**
     * A Butterworth order 2 filter designed to band pass 100-500Hz on a sample rate of
     * 16000Hz. See Tony Fisher's web page at York University for derivation
     * @return the filtered SoundWave.
     */
    public SoundWave filterButterworthFormantPassOrder2()
    {
        double[] filtered;
        if(frequency==16000)
        {
            filtered=DigitalFilter.ButterworthOrder2_16000Hz_BandPass_100_500Hz(this.data);
        }
        else if (frequency==11025)
        {
            filtered=DigitalFilter.ButterworthOrder2_11025Hz_BandPass_100_500Hz(this.data);
        }
        else throw new IllegalStateException("Don't have a filter for this frequency.");
        return new SoundWave(filtered, frequency);
    }

    /**
     * A Butterworth order 2 filter designed to band pass 100-500Hz on a sample rate of
     * 16000Hz. See Tony Fisher's web page at York University for derivation
     * @return the filtered SoundWave.
     */
    public SoundWave filterButterworthHissPassOrder2()
    {
        double[] filtered;
        if(frequency==16000)
        {
            filtered=DigitalFilter.ButterworthOrder2_16000Hz_BandPass_3000_5000Hz(this.data);
        }
        else if (frequency==11025)
        {
            filtered=DigitalFilter.ButterworthOrder2_11025Hz_BandPass_3000_5000Hz(this.data);
        }
        else throw new IllegalStateException("Don't have a filter for this frequency.");
        return new SoundWave(filtered, frequency);
    }


}