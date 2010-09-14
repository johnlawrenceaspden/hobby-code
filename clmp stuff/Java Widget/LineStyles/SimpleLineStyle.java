import java.awt.*;

/**
 * The simplest sort of graph line. Just has a colour.
 */
public class SimpleLineStyle extends LineStyle
{
	private Color theColour;
	
	public SimpleLineStyle(Color c)
	{
		theColour=c;
	}
	
	public void drawLine( Graphics g, int[] x, int[]y, boolean[] valid)
	{
		Color old=g.getColor();
		g.setColor(theColour);
		for(int i=1; i<x.length; i++)
		{
		    if(valid[i-1] && valid[i]) g.drawLine(x[i-1],y[i-1],x[i],y[i]);
		}
		g.setColor(old);
	}
}
