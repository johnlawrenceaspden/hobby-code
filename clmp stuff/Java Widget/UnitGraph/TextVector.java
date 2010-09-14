import java.util.*;
import java.awt.*;

/**
 * A vector of strings which can draw itself in a graphics context
 */
public class TextVector implements BoxObject
{
	private Vector theStrings;
	private int ascent, descent, width, height;
	
	public TextVector()
	{
		theStrings=new Vector();
	}
	
	public TextVector(String s)
	{
		this();
		addString(s);
	}
	
	public void addString(String s)
	{
		theStrings.addElement(s);
	}
	
	public void draw(Graphics g, int x, int y)
	{
		if(theStrings.isEmpty()) return;
		
		calculateSize(g);

		y+=ascent;
		
		for(Enumeration e=theStrings.elements();e.hasMoreElements();)
		{
			g.drawString((String)(e.nextElement()),x,y);
			y+=height;
		}
	}
	
	private void calculateSize(Graphics g)
	{
		FontMetrics fm = g.getFontMetrics();
				
		width=0;
		ascent=fm.getAscent();
		descent=fm.getDescent()+fm.getHeight()*(theStrings.size()-1);
		height=fm.getHeight();
						
		for(Enumeration e=theStrings.elements();e.hasMoreElements();)
		{
			int w=fm.stringWidth((String)(e.nextElement()));
			if(width < w) width=w;
		}
	}

	public int getWidth(Graphics g)
	{
		calculateSize(g);
		return width;
	}

	public int getDepth(Graphics g)
	{
		calculateSize(g);
		return ascent+descent;
	}
}

