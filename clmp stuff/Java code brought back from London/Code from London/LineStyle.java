import java.awt.Color;

class LineStyle
{
	public final static int 
		BLUEYELLOWSTRIPE=1,
		BLUELINE=2,
		REDLINE=3,
		THICKBLACK=4, 
		BLUEDASHED=5,
		THICKBLACKDASHED=6;
	int thickness;
	int striping;
	Color colour1;
	Color colour2;
	
	public LineStyle()
	{
		colour1=colour2=Color.black; thickness=0; striping=0;
	}

	public LineStyle(int i)
	{
		this();
		switch(i)
		{
		case BLUEYELLOWSTRIPE: colour1=Color.blue; colour2=Color.yellow; striping=4; break;
		case BLUELINE: colour1=Color.blue; break;
		case REDLINE: colour1=Color.red; break;
		case THICKBLACK: thickness=2; break;
		case THICKBLACKDASHED: colour2=Color.white; thickness=2; striping=4; break;
		case BLUEDASHED: colour1=Color.blue; colour2=Color.white; striping=1; break;
		}
	}
}
