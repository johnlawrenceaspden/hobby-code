import java.awt.*;

/**
 * A style of line where points are large and one colour,
 * and the lines between them are fine and another colour.
 */
public class BobblyLineStyle extends LineStyle
{
	private Color lineColour, pointColour;
	
	public BobblyLineStyle(Color lineColour, Color pointColour)
	{
		this.lineColour=lineColour;
		this.pointColour=pointColour;
	}
	
	public BobblyLineStyle(Color c)
	{
		this(c,c);
	}
    	
	public void drawLine( Graphics g, int[] x, int[]y, boolean[] valid)
	{
		Color old=g.getColor();
		g.setColor(lineColour);
		for(int i=1; i<x.length; i++)
		{
		    if(valid[i-1] && valid[i]) g.drawLine(x[i-1],y[i-1],x[i],y[i]);
		}
		g.setColor(pointColour);
		for(int i=0; i<x.length; i++)
		{
		    if(valid[i]) g.fillOval(x[i]-2,y[i]-2,4,4);
		}
		g.setColor(old);
	}
	
}
