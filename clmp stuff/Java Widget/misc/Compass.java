/**
 * Point of the compass.
 * new Compass(Compass.N) will create an object representing North
 * new Compass(Compass.C) will create an object representing the Centre
 */
public class Compass
{
	public static final int C=0,N=1,NE=2,E=3,SE=4,S=5,SW=6,W=7,NW=8;
	private static final int first=0, last=8;
	
	private int thePoint;
	
	public Compass(int point)
	{
		if(point<first || point>last)	throw new IllegalArgumentException();
		thePoint=point;
	}
	
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
	
	public Compass getOpposite()
	{
		int y;
		switch(thePoint)
		{
		case N:		y=S;	break;
		case NE:	y=SW;	break;
		case NW:	y=SE;	break;
		case C:		y=C;	break;
		case E:		y=W;	break; 
		case W:		y=E;	break;
		case SW:	y=NE;	break;
		case S:		y=N;	break; 
		case SE:
		default:	y=NW;
		}
		
		return new Compass(y);
	}

}
