/*
 * Colourizer.java
 */

package com.aspden.graph;

import java.awt.*;

/**
 */
public abstract class Colourizer {
    public abstract Color getColour(double x);

    public static class RedShade extends Colourizer{
        public Color getColour(double x)
        {
            if(x>1.0) return Color.green;
            else if(x< 0.0) return Color.green;
            else return new Color((int)(255*x),0,0);

        }
    }

    public static class GreyShade extends Colourizer{
        public Color getColour(double x)
        {
            if(x>1.0) return Color.green;
            else if(x< 0.0) return Color.green;
            else
            {
                int f=(int)(255*x);
                return new Color(f,f,f);
            }

        }
    }
    
    public static class Rank extends Colourizer{
        public Color getColour(double x)
        {
            System.out.println(" "+x);
            x-=0.000000000001;
            if(x>0.5) return Color.yellow;
            if(x>0.25) return Color.red;
            if(x>0.125) return Color.green;
            else return Color.black;
        }
    }

    public static class LogRainbow extends Colourizer{
        public Color getColour(double x)
        {
            if(x>1.0) return Color.white;
            else if(x< 0.0) return Color.white;
            else
            {
                int freq=(int)(x*1000);
                if(freq<=10) return new Color(freq*255/10,0,0); //red
                else if(freq<=30) return new Color(freq*255/30,freq*255/30,0); //yellow
                else if(freq<=100) return new Color(0,freq*255/100,0); //green
                else if(freq<=300) return new Color(0,0,freq*255/300); //blue
                else return new Color(freq*255/1000,0,freq*255/1000);//purple
            }

        }
    }

    public static class Palette extends Colourizer{
        private static final Color[] spectrum={
            Color.darkGray,Color.blue,Color.gray,Color.lightGray,Color.red,Color.magenta,Color.green,Color.cyan,Color.orange,Color.pink,Color.yellow,Color.white
        };
        public Color getColour(double x)
        {
            if(x>1.0) return Color.black;
            else if(x< 0.0) return Color.black;
            else
            {
                int index=(int)(x*spectrum.length);
                if(index==spectrum.length) index=spectrum.length-1;
                return spectrum[index];
            }

        }
    }


}

