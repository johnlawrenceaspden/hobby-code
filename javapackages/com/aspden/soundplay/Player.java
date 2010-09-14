/*
 * Player.java
 */

package com.aspden.soundplay;

import javax.sound.sampled.*;
import java.io.*;

/** Plays an array of integers or doubles (in the range -32768 to 32767) as sound.
 *
 */
public class Player extends Object {
    int[] samples;
    int frequency;

    /** Creates a Player for the sound represented by a set of integer samples.
     * @param samples Integers in the range (-32768, 32767) representing sampled sound.
     * @param frequency sampling frequency.
     */
    public Player(int[] samples,int frequency){
        System.out.println("player created 1");
        this.samples=samples;
        this.frequency=frequency;
    }

    /** Creates a Player for the sound represented by a set of doubles
     * @param samples Doubles in the range (-32768, 32767) representing sampled sound.
     * @param frequency sampling frequency.
     */
    public Player(double[] samples, int frequency) {
        this.samples=new int[samples.length];
        for(int i=0; i<samples.length; i++) this.samples[i]=(int) samples[i];
        this.frequency=frequency;
    }

    /** Plays the sound.
     * @throws LineUnavailableException if it can't obtain an audio output.
     */
    public void play() throws LineUnavailableException {
        playSamples(this.samples, this.frequency);
    }
    
    /** Plays the sound after rescaling it to be as loud as possible.
     * @throws LineUnavailableException if it can't get an audio output line.
     */
    public void playLoud() throws LineUnavailableException {
        int[] scaledsamples=new int[samples.length];
        int max=0;
        for(int i=0; i<samples.length; i++)
        {
            int s=samples[i];
            if(s<0) s=-s;
            if(s>max) max=s;
        }
        double scale=32767.0/(double) max;
        for(int i=0; i<samples.length; i++) scaledsamples[i]=(int)(samples[i]*scale);
        playSamples(scaledsamples, this.frequency);
    }
    
    
    private void playSamples(int[] samples, int frequency) throws LineUnavailableException
    {
        AudioFormat format=new AudioFormat(frequency, 16, 1, true, true); //16 bit mono signed big-endian.
        byte[] bytes=new byte[samples.length*2];
        for(int i=0; i<samples.length; i++)
        {
            int s=samples[i];

            int LSB=s&0x000000FF;
            int MSB=s&0x0000FF00;
            MSB=MSB>>8;
            byte L=(byte) LSB;
            byte M=(byte) MSB;

            bytes[i*2]=M;
            bytes[i*2+1]=L;
        }

        try{
            AudioInputStream ais = new AudioInputStream(new ByteArrayInputStream(bytes), format, samples.length);
            ais.reset();
            DataLine.Info a=new DataLine.Info(Clip.class, ais.getFormat());
            Clip theClip=(Clip)AudioSystem.getLine(a);
            theClip.open(ais);
            theClip.start();
            theClip.drain();
            while(theClip.isActive()) com.aspden.Stuff.threadSleep(1000); //It appears that drain() doesn't block until all the sound is gone!
            com.aspden.Stuff.threadSleep(1000);
            theClip.stop();
            theClip.close();
        }catch(IOException e){
            System.out.println(e);
            System.exit(1);
        }
    }

    /** Test/Demo code
     * @param args ignored
     * @throws LineUnavailableException if audio output is unavailable.
     */
    public static void main(String[] args) throws LineUnavailableException
    {
        double[] a =new double[30000];
        double freq=440; //play an A.
        for(int i=0; i<a.length; i++)
        {
            double x=((double)i/11025); //time in seconds.
            if(i>29000) x=((double)(2*i-29000))/11025;
            a[i]=10000*Math.sin(freq*2*Math.PI*x);
        }
        
        
        Player p=new Player(a, 11025);
        p.play();
        p.playLoud();
        
        Player q=new Player(a, 11025);
        q.play();
        q.playLoud();
        
        
        //SHOULDN'T NEED THIS. WHAT IS HANGING? 
        System.exit(0);
        
    }


}