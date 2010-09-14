import java.awt.*;

public class UnitGraphLegendBox extends UnitGraphObject
{
	private LegendVector theLegendVector;
	private Compass theCompass;
	private static final double theInset=0.9;
	
	
	public UnitGraphLegendBox(int compass)
	{
		theLegendVector=new LegendVector();
		theCompass=new Compass(compass);
	}
	
	public void addLegend(LineStyle l, String s)
	{
		theLegendVector.addLegend(l,s);
	}

	public void draw(Graphics g, int w, int h)
	{
		int x=(int)(w*(0.5+theCompass.getWE()*(theInset)/2));
		int y=(int)(h*(0.5-theCompass.getSN()*(theInset)/2));
		Compass direction=theCompass.getOpposite();
		
		(new BoxObjectWrapper(theLegendVector)).directedDraw(g,x,y,direction);
	}
}
