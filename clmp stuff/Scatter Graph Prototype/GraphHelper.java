import java.awt.*;
import java.util.*;

/**
 * Draws Scatter plots onto an in-memory image
 */
public class GraphHelper
{
	/**
	 * Data in as real coordinates
	 */
	Vector theLData=new Vector();
	Vector theRData=new Vector();
	/**
	 * Objects defining the plot area in real coords and in pixels
	 */
	Scaler xScaler, yLScaler, yRScaler;
	TextHelper theTextHelper;
	
	private void LastPoint(Graphics g, Points p, boolean lData)
	{
		Color old;
		old=g.getColor();
		
		g.setColor(p.getColour());
		Scaler s;
		
		if(lData){s=yLScaler;}	else {s=yRScaler;}

		{
			int x,y;
			x = xScaler.pixel(p.getX(p.getSize()-1));
			y = s.pixel(p.getY(p.getSize()-1));
			
			if(xScaler.isVisible(x) && s.isVisible(y))
			{
				theTextHelper.putLabelAtCoords(g,x,y,lData,!lData);
				g.fillOval(x-3,y-3,6,6);
			}
		}
		g.setColor(old);
	}
	
	private void GraphData(Graphics g, Points p, boolean lData)
	{
		Color old;
		old=g.getColor();
		
		g.setColor(p.getColour());
		Scaler s;
		
		if(lData){s=yLScaler;}	else {s=yRScaler;}

		for(int i=1; i < p.getSize(); i++)
		{
			int x1, y1, x2, y2;
			
			x1 = xScaler.pixel(p.getX(i-1));
			y1 = s.pixel(p.getY(i-1));
			x2 = xScaler.pixel(p.getX(i));
			y2 = s.pixel(p.getY(i));
			g.drawLine(x1,y1,x2,y2); 
		}
		
		g.setColor(old);
	}
	
	public void addGraphToImage(Image a)
	{
		Graphics g = a.getGraphics();
		
		Enumeration e=theLData.elements();
		while(e.hasMoreElements())
		{
			GraphData(g,(Points)e.nextElement(),true);
		}
		
		e=theRData.elements();
		while(e.hasMoreElements())
		{
			GraphData(g,(Points)e.nextElement(),false);
		}
	}
	
	public void addLastPointsToImage(Image a)
	{
		Graphics g = a.getGraphics();
		
		Enumeration e=theLData.elements();
		while(e.hasMoreElements())
		{
			LastPoint(g,(Points)e.nextElement(),true);
		}
		e=theRData.elements();
		while(e.hasMoreElements())
		{
			LastPoint(g,(Points)e.nextElement(),false);
		}
	}
	
	public void setLData(Vector v){theLData=v;}
	public void setRData(Vector v){theRData=v;}
	public void setXScaler(Scaler s){xScaler=s;}
	public void setYLScaler(Scaler s){yLScaler=s;}
	public void setYRScaler(Scaler s){yRScaler=s;}
	public void setTextHelper(TextHelper t){theTextHelper=t;}
}
