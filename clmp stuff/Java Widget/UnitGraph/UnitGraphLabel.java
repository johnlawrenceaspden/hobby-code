import java.awt.*;

public class UnitGraphLabel extends UnitGraphObject
{
	private TextVector theTextVector;
	private Compass theCompass;
	private static final double theInset=0.9;
	
	
	public UnitGraphLabel(int compass)
	{
		theTextVector=new TextVector();
		theCompass=new Compass(compass);
	}
	
	public UnitGraphLabel(int compass, String s)
	{
		this(compass);
		addString(s);
	}
	
	public void addString(String s)
	{
		theTextVector.addString(s);
	}

	public void draw(Graphics g, int w, int h)
	{
		int x=(int)(w*(0.5+theCompass.getWE()*(theInset)/2));
		int y=(int)(h*(0.5-theCompass.getSN()*(theInset)/2));
		Compass direction=theCompass.getOpposite();
		
		(new BoxObjectWrapper(theTextVector)).directedDraw(g,x,y,direction);
	}
}
