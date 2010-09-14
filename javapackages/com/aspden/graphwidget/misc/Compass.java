package com.aspden.graphwidget.misc;


/**
 * Point of the compass.
 * new Compass(Compass.N) will create an object representing North
 * new Compass(Compass.C) will create an object representing the Centre
 */
public class Compass
{
    /** Compass directions*/
    public static final int C=0,N=1,NE=2,E=3,SE=4,S=5,SW=6,W=7,NW=8;
    private static final int first=0, last=8;

    private int thePoint;

    /** Create a new Compass point.
     * @param point The direction of the point. e.g. Compass.N for North.
     */
    public Compass(int point)
    {
        if(point<first || point>last)	throw new IllegalArgumentException();
        thePoint=point;
    }

    /** Is the point Westerly, Easterly or Central?
     * e.g. N is central, NE is easterly.
     * @return -1 = Westerly, 0 for Central, 1 for Easterly
     */
    public int getWE()
    {
        switch(thePoint)
        {
            case Compass.S:	case Compass.C:	case Compass.N:   return 0;
            case Compass.W:	case Compass.NW: case Compass.SW: return -1;
            case Compass.SE: case Compass.E: case Compass.NE: return 1;
            default: return 0;
        }
    }

    /** Is the point Southerly, Northerly or Central?
     * e.g. N is northerly, as is NE.
     * @return -1 = Southerly, 0 for Central, 1 for Northerly.
     */
    public int getSN()
    {
        switch(thePoint)
        {
            case Compass.C: case Compass.E:	case Compass.W:   return 0;
            case Compass.N: case Compass.NE: case Compass.NW: return 1;
            case Compass.SW: case Compass.S: case Compass.SE: return -1;
            default: return 0;
        }
    }

    /** Get the opposite point.
     * @return The opposite point. NE<->SW.
     */
    public Compass getOpposite()
    {
        int y;
        switch(thePoint)
        {
            case N:	y=S;	break;
            case NE:	y=SW;	break;
            case NW:	y=SE;	break;
            case C:	y=C;	break;
            case E:	y=W;	break;
            case W:	y=E;	break;
            case SW:	y=NE;	break;
            case S:	y=N;	break;
            case SE:	y=NW;   break;
            default: throw new IllegalStateException("Something wrong in class Compass.");
        }

        return new Compass(y);
    }

    public boolean isCardinal()
    {
        return( isN() || isS() || isE() || isW());
    }

    public boolean isEW()
    {
        return(isE() || isW());
    }

    public boolean isNS()
    {
        return(isN() || isS());
    }

    public boolean isN()
    {
        return((thePoint==Compass.N));
    }

    public boolean isS()
    {
        return((thePoint==Compass.S));
    }

    public boolean isE()
    {
        return((thePoint==Compass.E));
    }

    public boolean isW()
    {
        return((thePoint==Compass.W));
    }

}
