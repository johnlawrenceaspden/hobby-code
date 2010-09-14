import java.awt.*;

/**
 * A wrapper which allows BoxObjects 
 * to draw themselves into a graphics context at
 * any compass direction relative to a given point
 */
public class BoxObjectWrapper
{
	private BoxObject theBoxObject;
	
	public BoxObjectWrapper(BoxObject a)
	{
		theBoxObject=a;
	}
	
	public void directedDraw(Graphics g, int x, int y, Compass direction)
	{
		int width=theBoxObject.getWidth(g);
		int depth=theBoxObject.getDepth(g);
		
		switch(direction.getWE())
		{
			case 0:
					x-=width/2;
					break;
			case -1:
					x-=width;
					break;
			default:
					break;
		}
		switch(direction.getSN())
		{
			case 1:
					y-=depth;
					break;
			case 0:
					y-=depth/2;
					break;
			default:
					
					break;
		}
		
		theBoxObject.draw(g,x,y);

	}
}
