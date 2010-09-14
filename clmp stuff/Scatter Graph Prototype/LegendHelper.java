import java.awt.*;
import java.util.*;

public class LegendHelper
{
	TextHelper theTextHelper;
	Font theFont;
	Scaler xScaler,yScaler;
	Vector lData,rData;

	
	public LegendHelper(TextHelper th, Scaler x, Scaler y, Vector l, Vector r)
	{
		theTextHelper=th;
		xScaler=x;
		yScaler=y;
		lData=l;
		rData=r;
	}
	
	public void setFont(Font f)
	{
		theFont=f;
	}
	
	public void addLegendsToImage(Image img)
	{
		int linelength=10;
		Vector l = new Vector();
		
		for(Enumeration e=lData.elements();e.hasMoreElements();)
		{
			Points p=(Points)e.nextElement();
			StyledLegend sl = new StyledLegend(p.getLegend(), p.getColour());
			l.addElement(sl);
		}
		for(Enumeration e=rData.elements();e.hasMoreElements();)
		{
			Points p=(Points)e.nextElement();
			StyledLegend sl = new StyledLegend(p.getLegend(), p.getColour());
			l.addElement(sl);
		}
		
		Graphics g = img.getGraphics();
		if(theFont!=null) g.setFont(theFont);
		
		FontMetrics fm = g.getFontMetrics();
		
		int boxWidth=0;
		int boxAscent=fm.getAscent();
		int boxDescent=fm.getDescent()+fm.getHeight()*(l.size()-1);
				
		for(Enumeration e=l.elements();e.hasMoreElements();)
		{
			StyledLegend sl =(StyledLegend) e.nextElement();
			int w=fm.stringWidth(sl.s);
			if(boxWidth < w) boxWidth=w;
		}
		
		boxWidth+=linelength;
		
		int x=xScaler.getMinPixel();
		int y=yScaler.getMinPixel();
		
		x=theTextHelper.fitInterval(x,0,boxWidth,xScaler.getMaxPixel()/20,19*xScaler.getMaxPixel()/20);
		y=theTextHelper.fitInterval(y,boxAscent, boxDescent,yScaler.getMinPixel()/20,19*yScaler.getMinPixel()/20);
		
		{
			Color old=g.getColor();
			g.setColor(Color.gray);
			g.clearRect(x,y-boxAscent, boxWidth, boxAscent+boxDescent);
			g.draw3DRect(x,y-boxAscent, boxWidth, boxAscent+boxDescent, true);
			g.setColor(old);
		}
		
		for(Enumeration e=l.elements();e.hasMoreElements();)
		{
			StyledLegend sl =(StyledLegend) e.nextElement();
			Color old=g.getColor();
			g.setColor(sl.c);
			g.drawLine(x,y-fm.getAscent()/2,x+linelength,y-fm.getAscent()/2);
			g.setColor(old);
			g.drawString(sl.s,x+linelength,y);
			y+=fm.getHeight();
		}
	}
	
	private class StyledLegend
	{
		String s;
		Color c;
		
		public StyledLegend(String s, Color c)
		{
			this.s=s;
			this.c=c;
		}
	}
	
}