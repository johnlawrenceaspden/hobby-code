/*
 * SoundCatcher.java
 */

package com.aspden.soundrecorder;

import javax.sound.sampled.*;
import com.aspden.graph.*;

/**A soundcatcher will capture a certain amount of sound at a certain sample frequency and stuff it into a public buffer as integer samples.
It is its own thread and must be created and then started with start()*/
public class SoundCatcher extends Thread {
    private int frequency;

    private int[] theBuffer; //Sound samples written to here.
    private int writePosition;

    private boolean finish; // variable used to kill the thread. Can be set by finish method.

    private TargetDataLine theLine; //Sound input

    /** Creates a new SoundCatcher.
     * @param frequency Desired sampling frequency
     * @param seconds No of seconds to put into buffer before stopping.
     * @throws LineUnavailableException If the system is unable to provide a sound input of the desired frequency.
     * Could be because another program has the input.
     */
    public SoundCatcher(int frequency,int seconds) throws LineUnavailableException {
        this.theBuffer=new int[seconds*frequency];
        writePosition=0;
        //Acquire a 16 bit, mono, signed, big-endian sound input from the system
        AudioFormat theFormat=new AudioFormat(frequency,16,1,true,true);
        theLine= (TargetDataLine) AudioSystem.getLine(new DataLine.Info(TargetDataLine.class, theFormat));
        theLine.open(theFormat);
    }

    /**read in data until the buffer is full or finish() is called.*/
    public void run()
    {
        byte[] rawbuffer = new byte[theLine.getBufferSize()/500*100]; //bodge to get round numbers from buffer
        writePosition=0;

        theLine.start();

        finish=false; //finish is either set in the loop by buffer overrun, or prematurely by calling finish.
        while(!finish)
        {
            com.aspden.Stuff.threadSleep(1);
            int numread = theLine.read(rawbuffer, 0, rawbuffer.length);

            int samples=numread/2;
            int rawbufferpos=0;
            for(int i=0; i<samples; i++)
            {
                byte MSB=rawbuffer[rawbufferpos++];
                byte LSB=rawbuffer[rawbufferpos++];
                theBuffer[writePosition]=bytesToInt(MSB,LSB);
                writePosition++;
                if(writePosition>=theBuffer.length)
                {
                    finish=true;
                    break;
                }
            }
        }
        theLine.stop();
        com.aspden.Stuff.threadSleep(1); //This seems to stop a locking problem.
        theLine.close();

    }

    private int bytesToInt(byte MSB, byte LSB)
    {
        return (MSB << 8) | (LSB & 0xFF);
    }

    /** Gets buffer position.
     * Asynchronously updated by the sound catching thread
     * @return the position in the buffer where the next sound sample will be written.
     */
    public int getWritePosition() {
        return writePosition;
    }

    /** Exposes the sound catching buffer.
     * @return A reference to the integer array where sound samples are being written by this thread.
     */
    public int[] getBuffer() {
        return theBuffer;
    }

    /** Stop catching sound.
     */
    public void finish() {
        finish=true;
    }

    /** Test/Demo creates a soundcatcher, and uses it to catch sound which is then displayed.
     * @param args Command line parameters - ignored
     * @throws LineUnavailableException Program just crashes if it can't get a line.
     */
    public static void main(String args[]) throws LineUnavailableException
    {

        SoundCatcher s= new SoundCatcher(44100, 1); //CD quality, 1 second

        s.start(); //begin sound capture

        //While the thread is running, display the write position every time it goes up by 1000.
        int lastpos=0;

        while(s.isAlive())
        {
            com.aspden.Stuff.threadSleep(1);
            int newpos=s.getWritePosition();
            if (newpos>lastpos+1000)
            {
                System.out.println(newpos);
                lastpos=newpos;
            }
        }

        //Convert the ints in the buffer into floats for the graph component
        int len=s.getWritePosition();
        int[] buf=s.getBuffer();

        double[] sound=new double[len];
        for(int i=0; i<len;i++)
        {
            sound[i]=buf[i];
        }

        //Display the captured wave.
        new SingleComponentFrame(new ArrayDisplayPanel(sound),"Captured Sound");
    }
}



