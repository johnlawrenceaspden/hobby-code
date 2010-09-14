import java.awt.*;
import java.util.*;

/**
 * Holds a vector of strings and associated line styles
 * which can display themselves as a legend box.
 * Implements BoxObject in order that it may be decorated with the 
 * ability to draw itself relative to a point, using BoxObjectWrapper
 */
public class LegendVector implements BoxObject
{
	private static final int lineLength=10; //Length of example lines
	private static final int separation=5;  //example to text distance
	
	private int textAscent, textDescent, textWidth, textHeight;
	
	private Vector theStrings;
	private Vector theLineStyles;
	private int theLineLength;
	
	public LegendVector()
	{
		theStrings=new Vector();
		theLineStyles=new Vector();
	}
	
	public void addLegend(LineStyle l, String s)
	{
		theStrings.addElement(s);
		theLineStyles.addElement(l);
	}
	
	public void draw(Graphics g, int x, int y)
	{
		if(theStrings.isEmpty()) return;
		
		calculateSize(g);
		
		{
			int w=textWidth+lineLength+separation;
			int h=textAscent+textDescent;
			
			Color c= g.getColor();
			g.setColor(Color.gray);
			g.clearRect(x,y, w,h);
			g.draw3DRect(x-1,y-1,w+1,h+1, true);
			g.setColor(c);
		}

		y+=textAscent;
		x+=lineLength+separation;
		Enumeration se=theStrings.elements();
		Enumeration le=theLineStyles.elements();
		while (se.hasMoreElements())
		{
			LineStyle	l = (LineStyle)	le.nextElement();
			String		s = (String)	se.nextElement();
			
			g.drawString(s,x,y);
			{
				int x1=x-separation;
				int x2=x-separation-lineLength;
				int y0=y-textAscent/2;
				l.drawLine(g,x1,y0,x2,y0);
			}
			y+=textHeight;
		}
	}
	
	private void calculateSize(Graphics g)
	{
		FontMetrics fm = g.getFontMetrics();
				
		textWidth=0;
		textAscent=fm.getAscent();
		textDescent=fm.getDescent()+fm.getHeight()*(theStrings.size()-1);
		textHeight=fm.getHeight();
						
		for(Enumeration e=theStrings.elements();e.hasMoreElements();)
		{
			int w=fm.stringWidth((String)(e.nextElement()));
			if(textWidth < w) textWidth=w;
		}
	}

	public int getWidth(Graphics g)
	{
		calculateSize(g);
		return textWidth+separation+lineLength;
	}

	public int getDepth(Graphics g)
	{
		calculateSize(g);
		return textAscent+textDescent;
	}

}
