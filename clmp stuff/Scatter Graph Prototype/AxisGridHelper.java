import java.awt.*;
import java.util.*;

/**
 * A GridHelper, given a pair of Scaler objects and an image to draw on,
 * will work out the best positions for gridlines to be in.
 */


public class AxisGridHelper
{
	/**
	 * Scaler objects to tell us about the image size 
	 * in real coords and in pixels
	 */
	Scaler xScaler, yLScaler, yRScaler;
	TextHelper theTextHelper;
	int xSpacing, ySpacing;

	/**
	 * Work out a grid based on the scaler objects 
	 * and add it to the image
	 */
	private void addGridToImage(Image a, Scaler yScaler)
	{
		Graphics g = a.getGraphics();
		
		Vector positions;
		
		Color old = g.getColor();
		g.setColor(Color.lightGray);
		
		
		positions = xScaler.cardinalPositions(xSpacing);
		
		for (Enumeration e = positions.elements() ; e.hasMoreElements();) 
		{
			double x=((Double)e.nextElement()).doubleValue();
			
			int x0,y1,y2;
			x0=xScaler.pixel(x);
			y1=yScaler.getMinPixel();
			y2=yScaler.getMaxPixel();
			
			g.drawLine(x0,y1,x0,y2);
		}
		
		positions = yScaler.cardinalPositions(ySpacing);
		for (Enumeration e = positions.elements() ; e.hasMoreElements();) 
		{
			double y=((Double)e.nextElement()).doubleValue();
			
			int y0,x1,x2;
			y0=yScaler.pixel(y);
			x1=xScaler.getMinPixel();
			x2=xScaler.getMaxPixel();
			
			g.drawLine(x1,y0,x2,y0);
		}

		g.setColor(old);
	}
	
	
	public void addLGridToImage(Image a)
	{
		addGridToImage(a,yLScaler);
	}
	
	public void addRGridToImage(Image a)
	{
		addGridToImage(a,yRScaler);
	}

	private void drawXAxis(Graphics g)
	{
		int y0,x1,x2;
		y0=yLScaler.getMinPixel();
		x1=xScaler.getMinPixel();
		x2=xScaler.getMaxPixel();
				
		g.drawLine(x1,y0,x2,y0);
	}
	
	private void drawYLAxis(Graphics g)
	{
		int x0,y1,y2;
		x0=xScaler.getMinPixel();
		y1=yLScaler.getMinPixel();
		y2=yLScaler.getMaxPixel();
			
		g.drawLine(x0,y1,x0,y2);
	}
	private void drawYRAxis(Graphics g)
	{
		int x0,y1,y2;
		x0=xScaler.getMaxPixel();
		y1=yRScaler.getMinPixel();
		y2=yRScaler.getMaxPixel();
			
		g.drawLine(x0,y1,x0,y2);
	}
	
	private void drawXTicks(Graphics g)
	{
		
		Vector positions = xScaler.cardinalPositions(xSpacing);
		for (Enumeration e = positions.elements() ; e.hasMoreElements();) 
		{
			double x=((Double)e.nextElement()).doubleValue();
			
			theTextHelper.drawXAxisLabel(g,x);
			
			int x0=xScaler.pixel(x);
			int y1=yLScaler.getMinAxisTickPixel(true);
			int y2=yLScaler.getMinAxisTickPixel(false);
			
			g.drawLine(x0,y1,x0,y2);
		}
	}
	
	private void drawYLTicks(Graphics g)
	{	
		Vector positions = yLScaler.cardinalPositions(ySpacing);
		Enumeration e = positions.elements();
		
		if(e.hasMoreElements()) e.nextElement(); //miss first one. conflicts with x axis label
		while( e.hasMoreElements()) 
		{
			double y=((Double)e.nextElement()).doubleValue();
			
			theTextHelper.drawYLAxisLabel(g,y);
			
			int y0=yLScaler.pixel(y);
			int x1=xScaler.getMinAxisTickPixel(true);
			int x2=xScaler.getMinAxisTickPixel(false);
			
			g.drawLine(x1,y0,x2,y0);
		}
	}
	
	private void drawYRTicks(Graphics g)
	{	
		Vector positions = yRScaler.cardinalPositions(ySpacing);
		
		Enumeration e = positions.elements();
		
		if(e.hasMoreElements()) e.nextElement(); //miss first one. conflicts with x axis label
		while( e.hasMoreElements()) 
		{
			double y=((Double)e.nextElement()).doubleValue();
			
			theTextHelper.drawYRAxisLabel(g,y);
			
			int y0=yRScaler.pixel(y);
			int x1=xScaler.getMaxAxisTickPixel(true);
			int x2=xScaler.getMaxAxisTickPixel(false);
			
			g.drawLine(x1,y0,x2,y0);
		}
	}

		




	/**
	 * Work out a grid based on the scaler objects 
	 * and add it to the image
	 */
	public void addAxesToImage(Image a, boolean laxis, boolean raxis)
	{
		Graphics g = a.getGraphics();
		
		Color old = g.getColor();
		g.setColor(Color.black);
		
		drawXAxis(g);
		drawXTicks(g);
		
		if(laxis)
		{
			drawYLAxis(g);
			drawYLTicks(g);
		}
		if(raxis)
		{
			drawYRTicks(g);
			drawYRAxis(g);
		}
		
		g.setColor(old);
	}


	public AxisGridHelper(Scaler x, Scaler yL, Scaler yR, TextHelper t, int xSpacing, int ySpacing)
	{
		this.xSpacing=xSpacing;
		this.ySpacing=ySpacing;
		xScaler=x;
		yRScaler=yR;
		yLScaler=yL;
		theTextHelper=t;
	}
}
