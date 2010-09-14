/*
 * DigitalFilter.java
 */

package com.aspden.tespar.utility;

/**
 * @see <a href="http://www-users.cs.york.ac.uk/~fisher/"> Tony Fisher's home page. Filter design programs.</a>
 */
public class DigitalFilter extends Object {

    /** A Butterworth order 1 filter designed to band pass 120-4300Hz on a sample rate of 16000Hz.
     *  @return filtered data.
     */
    public static double[] ButterworthOrder1_16000Hz_BandPass_120_4300Hz(double[] data)
    {
        double[] filtered=new double[data.length];
        double xv[] = new double[3];
        double yv[] = new double[3];

        for(int i=0;i<data.length;i++)
        {
            xv[0] = xv[1]; xv[1] = xv[2];
            xv[2] = data[i]/ 1.812467443e+00;
            yv[0] = yv[1]; yv[1] = yv[2];
            yv[2] =   (xv[2] - xv[0]) + (0.0353576406 * yv[0]) + (  0.9147994621 * yv[1]);
            filtered[i] = yv[2];
        }
        return filtered;
    }

    /** A Butterworth order 1 filter designed to band pass 120-4300Hz on a sample rate of 11025Hz.
     *  @return filtered data.
     */
    public static double[] ButterworthOrder1_11025Hz_BandPass_120_4300Hz(double[] data)
    {
        double[] filtered=new double[data.length];
        double xv[] = new double[3];
        double yv[] = new double[3];

        for(int i=0;i<data.length;i++)
        {
            xv[0] = xv[1]; xv[1] = xv[2];
            xv[2] = data[i] / 1.366946709e+00;
            yv[0] = yv[1]; yv[1] = yv[2];
            yv[2] = (xv[2] - xv[0]) + (  0.4295284258 * yv[0]) + (  0.4714510634 * yv[1]);
            filtered[i]  = yv[2];
        }

        return filtered;
    }

    /**
     * A Butterworth order 8 filter designed to high pass above 300Hz on a sample rate of
     * 16000Hz.
     * @return filtered data.
     */
    public static double[] ButterworthOrder8_16000Hz_HighPass_300Hz(double[] data)
    {
        double[] filtered=new double[data.length];
        double xv[] = new double[9];
        double yv[] = new double[9];

        for(int i=0;i<data.length;i++)
        {
            xv[0] = xv[1]; xv[1] = xv[2]; xv[2] = xv[3]; xv[3] = xv[4]; xv[4] = xv[5]; xv[5] = xv[6]; xv[6] = xv[7]; xv[7] = xv[8];
            xv[8] = data[i] / 1.352781958e+00;
            yv[0] = yv[1]; yv[1] = yv[2]; yv[2] = yv[3]; yv[3] = yv[4]; yv[4] = yv[5]; yv[5] = yv[6]; yv[6] = yv[7]; yv[7] = yv[8];
            yv[8] =   (xv[0] + xv[8]) - 8 * (xv[1] + xv[7]) + 28 * (xv[2] + xv[6])
            - 56 * (xv[3] + xv[5]) + 70 * xv[4]
            + ( -0.5464424063 * yv[0]) + (  4.7015005790 * yv[1])
            + (-17.7105410400 * yv[2]) + ( 38.1525149290 * yv[3])
            + (-51.4088571270 * yv[4]) + ( 44.3696508080 * yv[5])
            + (-23.9539902620 * yv[6]) + (  7.3961644907 * yv[7]);
            filtered[i] = yv[8];
        }
        return filtered;
    }

    /**
     * A Butterworth order 8 filter designed to high pass above 300Hz on a sample rate of
     * 11025Hz.
     * @return filtered data.
     */
    public static double[] ButterworthOrder8_11025Hz_HighPass_300Hz(double[] data)
    {
        double[] filtered=new double[data.length];
        double xv[] = new double[9];
        double yv[] = new double[9];

        for(int i=0;i<data.length;i++)
        {
            xv[0] = xv[1]; xv[1] = xv[2]; xv[2] = xv[3]; xv[3] = xv[4]; xv[4] = xv[5]; xv[5] = xv[6]; xv[6] = xv[7]; xv[7] = xv[8];
            xv[8] = data[i] / 1.550966588e+00;
            yv[0] = yv[1]; yv[1] = yv[2]; yv[2] = yv[3]; yv[3] = yv[4]; yv[4] = yv[5]; yv[5] = yv[6]; yv[6] = yv[7]; yv[7] = yv[8];

            yv[8] =   (xv[0] + xv[8]) - 8 * (xv[1] + xv[7]) + 28 * (xv[2] + xv[6]) - 56 * (xv[3] + xv[5]) + 70 * xv[4]
            + ( -0.4157144458 * yv[0]) + (  3.6899875278 * yv[1])
            + (-14.3513710380 * yv[2]) + ( 31.9455283100 * yv[3])
            + (-44.5164561570 * yv[4]) + ( 39.7699187280 * yv[5])
            + (-22.2456381650 * yv[6]) + (  7.1237447600 * yv[7]);
            filtered[i] = yv[8];
        }
        return filtered;
    }

    public static double[] ButterworthOrder8_16000Hz_LowPass_4300Hz(double[] data)
    {
        double[] filtered=new double[data.length];
        double xv[] = new double[9];
        double yv[] = new double[9];

        for(int i=0;i<data.length;i++)
        {
            xv[0] = xv[1]; xv[1] = xv[2]; xv[2] = xv[3]; xv[3] = xv[4]; xv[4] = xv[5]; xv[5] = xv[6]; xv[6] = xv[7]; xv[7] = xv[8];
            xv[8] = data[i] / 6.848084080e+01;
            yv[0] = yv[1]; yv[1] = yv[2]; yv[2] = yv[3]; yv[3] = yv[4]; yv[4] = yv[5]; yv[5] = yv[6]; yv[6] = yv[7]; yv[7] = yv[8];
            yv[8] =   (xv[0] + xv[8]) + 8 * (xv[1] + xv[7]) + 28 * (xv[2] + xv[6])
            + 56 * (xv[3] + xv[5]) + 70 * xv[4]
            + ( -0.0002454722 * yv[0]) + ( -0.0027496511 * yv[1])
            + ( -0.0275214966 * yv[2]) + ( -0.0815253526 * yv[3])
            + ( -0.3636411728 * yv[4]) + ( -0.4618619719 * yv[5])
            + ( -1.2044657770 * yv[6]) + ( -0.5962609641 * yv[7]);
            filtered[i] = yv[8];
        }
        return filtered;
    }

    public static double[] ButterworthOrder8_11025Hz_LowPass_4300Hz(double[] data)
    {
        double[] filtered=new double[data.length];
        double xv[] = new double[9];
        double yv[] = new double[9];

        for(int i=0;i<data.length;i++)
        {
            xv[0] = xv[1]; xv[1] = xv[2]; xv[2] = xv[3]; xv[3] = xv[4]; xv[4] = xv[5]; xv[5] = xv[6]; xv[6] = xv[7]; xv[7] = xv[8];
            xv[8] = data[i] / 6.161809216e+00;
            yv[0] = yv[1]; yv[1] = yv[2]; yv[2] = yv[3]; yv[3] = yv[4]; yv[4] = yv[5]; yv[5] = yv[6]; yv[6] = yv[7]; yv[7] = yv[8];

            yv[8] =   (xv[0] + xv[8]) + 8 * (xv[1] + xv[7]) + 28 * (xv[2] + xv[6])
            + 56 * (xv[3] + xv[5]) + 70 * xv[4]
            + ( -0.0263380453 * yv[0]) + ( -0.3038156421 * yv[1])
            + ( -1.5613580243 * yv[2]) + ( -4.6799951227 * yv[3])
            + ( -8.9748029012 * yv[4]) + (-11.3186974910 * yv[5])
            + ( -9.2164733971 * yv[6]) + ( -4.4647586414 * yv[7]);
            filtered[i] = yv[8];
        }
        return filtered;
    }

    public static double[] ButterworthOrder2_16000Hz_LowPass_4300Hz(double[] data)
    {
        double[] filtered=new double[data.length];
        double xv[] = new double[3];
        double yv[] = new double[3];

        for(int i=0;i<data.length;i++)
        {
            xv[0] = xv[1]; xv[1] = xv[2];
            xv[2] = data[i]/ 3.046350742e+00;
            yv[0] = yv[1]; yv[1] = yv[2];
            yv[2] =   (xv[0] + xv[2]) + 2 * xv[1] + ( -0.1749463096 * yv[0]) + ( -0.1381001314 * yv[1]);
            filtered[i] = yv[2];
        }
        return filtered;
    }

    public static double[] ButterworthOrder2_11025Hz_LowPass_4300Hz(double[] data)
    {
        double[] filtered=new double[data.length];
        double xv[] = new double[3];
        double yv[] = new double[3];

        for(int i=0;i<data.length;i++)
        {
            xv[0] = xv[1]; xv[1] = xv[2];
            xv[2] = data[i]/ 1.638592426e+00;
            yv[0] = yv[1]; yv[1] = yv[2];
            yv[2] =   (xv[0] + xv[2]) + 2 * xv[1] + ( -0.3786930752 * yv[0]) + ( -1.0624264709 * yv[1]);
            filtered[i] = yv[2];
        }
        return filtered;
    }

    public static double[] ButterworthOrder2_11025Hz_BandPass_100_500Hz(double[] data)
    {
        double[] filtered=new double[data.length];
        double xv[] = new double[5];
        double yv[] = new double[5];

        for(int i=0;i<data.length;i++)
        {
            xv[0] = xv[1]; xv[1] = xv[2]; xv[2] = xv[3]; xv[3] = xv[4];
            xv[4] = data[i] / 8.913099849e+01;
            yv[0] = yv[1]; yv[1] = yv[2]; yv[2] = yv[3]; yv[3] = yv[4];
            yv[4] =   (xv[0] + xv[4]) - 2 * xv[2]
            + ( -0.7244344274 * yv[0]) + (  3.1032104402 * yv[1])
            + ( -5.0288549456 * yv[2]) + (  3.6498531433 * yv[3]);
            filtered[i] = yv[4];
        }
        return filtered;
    }

    public static double[] ButterworthOrder2_16000Hz_BandPass_100_500Hz(double[] data)
    {
        double[] filtered=new double[data.length];
        double xv[] = new double[5];
        double yv[] = new double[5];

        for(int i=0;i<data.length;i++)
        {
            xv[0] = xv[1]; xv[1] = xv[2]; xv[2] = xv[3]; xv[3] = xv[4];
            xv[4] = data[i] / 1.793313528e+02;

            yv[0] = yv[1]; yv[1] = yv[2]; yv[2] = yv[3]; yv[3] = yv[4];
            yv[4] =   (xv[0] + xv[4]) - 2 * xv[2]
            + ( -0.8008026467 * yv[0]) + (  3.3671867253 * yv[1])
            + ( -5.3304807507 * yv[2]) + (  3.7640433184 * yv[3]);
            filtered[i] = yv[4];
        }
        return filtered;
    }

    public static double[] ButterworthOrder2_11025Hz_BandPass_3000_5000Hz(double[] data)
    {
        double[] filtered=new double[data.length];
        double xv[] = new double[5];
        double yv[] = new double[5];

        for(int i=0;i<data.length;i++)
        {
            xv[0] = xv[1]; xv[1] = xv[2]; xv[2] = xv[3]; xv[3] = xv[4];
            xv[4] = data[i] / 5.630712862e+00;
            yv[0] = yv[1]; yv[1] = yv[2]; yv[2] = yv[3]; yv[3] = yv[4];
            yv[4] =   (xv[0] + xv[4]) - 2 * xv[2]
            + ( -0.2176976303 * yv[0]) + ( -0.7299188718 * yv[1])
            + ( -1.5406390358 * yv[2]) + ( -1.9395566820 * yv[3]);
            filtered[i] = yv[4];
        }
        return filtered;
    }

    public static double[] ButterworthOrder2_16000Hz_BandPass_3000_5000Hz(double[] data)
    {
        double[] filtered=new double[data.length];
        double xv[] = new double[5];
        double yv[] = new double[5];

        for(int i=0;i<data.length;i++)
        {
            xv[0] = xv[1]; xv[1] = xv[2]; xv[2] = xv[3]; xv[3] = xv[4];
            xv[4] = data[i] / 1.024264069e+01;

            yv[0] = yv[1]; yv[1] = yv[2]; yv[2] = yv[3]; yv[3] = yv[4];

            yv[0] = yv[1]; yv[1] = yv[2]; yv[2] = yv[3]; yv[3] = yv[4];
            yv[4] =   (xv[0] + xv[4]) - 2 * xv[2]
            + ( -0.3333333333 * yv[0]) + ( -0.0000000000 * yv[1])
            + ( -0.9428090416 * yv[2]) + ( -0.0000000000 * yv[3]);
            filtered[i] = yv[4];
        }
        return filtered;
    }








}