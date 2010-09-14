/**
 * A component which contains UnitGraph objects and displays them.
 * It conceptually has a coordinate system [0,1]x[0,1]
 * with origin in the bottom left corner, but can assume any shape 
 * on the screen.
 */
import java.awt.*;
import java.awt.event.*;
import java.util.*;

public class UnitGraph extends BufferedCanvas
{
	Vector theUnitGraphObjects=new Vector();

	public void addElement(UnitGraphObject p)
	{
		theUnitGraphObjects.addElement(p);
	}
	
	public void clear()
	{
		theUnitGraphObjects=new Vector();
	}
	
	/**
	 * We display all the objects which have been added in order,
	 * with the twist that Axis objects are given the chance to draw 
	 * their gridlines first.
	 */
	protected void paintImage(Graphics g)
	{
		for(Enumeration e=theUnitGraphObjects.elements();e.hasMoreElements();)
		{
			UnitGraphObject o = (UnitGraphObject) e.nextElement();
			if (o instanceof UnitGraphAxis)
			{
				((UnitGraphAxis)o).drawGrid(g,getSize().width,getSize().height);
			}
		}
		for(Enumeration e=theUnitGraphObjects.elements();e.hasMoreElements();)
		{
			UnitGraphObject o = (UnitGraphObject) e.nextElement();
			o.draw(g,getSize().width,getSize().height);
		}
	}
	
	protected double xCoord(int x)
	{
		return (double)x/getSize().width;
	}
	
	protected double yCoord(int y)
	{
		return 1-((double)y/getSize().height);
	}

}
