import java.text.*;
import java.awt.*;	
import java.util.*;
				 

/** 
 * Rather ill defined object to perform any task relating
 * to text and the display of text.
 */
public class TextHelper
{
	Scaler xScaler, yLScaler, yRScaler;
	
	public void setXScaler(Scaler s){xScaler=s;}
	public void setYLScaler(Scaler s){yLScaler=s;}
	public void setYRScaler(Scaler s){yRScaler=s;}
	
	private static final int xTickToLabelSpacing=1;
	private static final int yTickToLabelSpacing=3;


	public String getCoordLabel(double x, double yl, double yr, boolean left, boolean right)
	{
		String s="";
		
		if(left) s+=getCoordText(yLScaler,yl)+" << ";
		s += getCoordText(xScaler,x);
		if(right) s+= " >> "+getCoordText(yRScaler,yr);
		
		return s;
	}
	
	public String getCoordText(Scaler s, double x)
	{
		//How many s.f?
		double range=Math.abs(s.getMaxCoord()-s.getMinCoord());
		double number=100;
		String format="0.";
		
		while(number>range)
		{
			format+="0";
			number/=10;
		}
		DecimalFormat nf= new DecimalFormat(format);
		return nf.format(x);
	}
	
	void approxDrawStringsOnBox(Graphics g, Vector vstr, int x, int y, boolean inset)
	{
		if (vstr.isEmpty()) return;
		
		FontMetrics fm = g.getFontMetrics();
		
		int boxWidth=0;
		int boxAscent=fm.getAscent();
		int boxDescent=fm.getDescent()+fm.getHeight()*(vstr.size()-1);
				
		for(Enumeration e=vstr.elements();e.hasMoreElements();)
		{
			int w=fm.stringWidth((String)(e.nextElement()));
			if(boxWidth < w) boxWidth=w;
		}
		
		if(inset)
		{
			x=fitInterval(x,0,boxWidth,xScaler.getMaxPixel()/20,19*xScaler.getMaxPixel()/20);
			y=fitInterval(y,boxAscent, boxDescent,yLScaler.getMinPixel()/20,19*yLScaler.getMinPixel()/20);
		}
		else
		{
			x=fitInterval(x,0,boxWidth,0,xScaler.getMaxPixel());
			y=fitInterval(y,boxAscent, boxDescent,0,yLScaler.getMinPixel());
		}
	
		g.clearRect(x, y-boxAscent, boxWidth, boxDescent+boxAscent);

		for(Enumeration e=vstr.elements();e.hasMoreElements();)
		{
			g.drawString((String)(e.nextElement()),x,y);
			y+=fm.getHeight();
		}
	}
	
	/**
	 * Moves x to make [x-s,x+h] to fit into range [min,max]
	 * if possible
	 */
	public int fitInterval(int x, int s, int h, int min, int max)
	{
		if(max<min){int t=min; min=max; max=t;}
		
		int excess=x+h-max;
		if(excess>0) x-=excess;
		
		excess=min-x+s;
		if(excess>0) x+=excess;
		
		return x;
		
	}
	
	/**
	 * Draws s as close to (x,y) as possible
	 */
	void approxDrawString(Graphics g, String s, int x, int y)
	{
		FontMetrics fm = g.getFontMetrics();
		
		x=fitInterval(x,0,fm.stringWidth(s),0,xScaler.getMaxPixel());
		y=fitInterval(y,fm.getAscent(), fm.getDescent(),0,yLScaler.getMinPixel());
		
		g.drawString(s,x,y);
		
	}

	private StringAndPixel directedDrawString(Graphics g, String s, int x, int y, int pos)
	{
		FontMetrics fm=g.getFontMetrics();
		int sw=fm.stringWidth(s);
		int a=fm.getAscent();
		int d=fm.getDescent();
		
		switch(pos)
		{
			case Compass.S:
			case Compass.C:
			case Compass.N:
					x-=sw/2;
					break;
			case Compass.W:
			case Compass.NW:
			case Compass.SW:
					x-=sw;
					break;
			case Compass.SE:
			case Compass.E:
			case Compass.NE:
					break;
		}
		switch(pos)
		{
			case Compass.N:
			case Compass.NE:
			case Compass.NW:
					y-=d;
					break;
			case Compass.C:
			case Compass.E:
			case Compass.W:
					y+=(a-d)/2;
					break;
			case Compass.SW:
			case Compass.S:
			case Compass.SE:
					y+=a;
					break;
		}
		
		return new StringAndPixel(s,x,y);
		
	}
	
	public void putLabelAtCoords(Graphics g, int x, int y, boolean left, boolean right)
	{
		double xc = xScaler.coord(x);
		double ylc = yLScaler.coord(y);
		double yrc = yRScaler.coord(y);
		String t=getCoordLabel(xc, ylc, yrc, left, right);
		
		Vector v=new Vector();
		v.addElement(t);
		
		approxDrawStringsOnBox(g,v,x,y,false);

	}
	
	public void drawXAxisLabel(Graphics g, double x)
	{
		String t=getCoordText(xScaler, x);
		int xp=xScaler.pixel(x);
		int yp=yLScaler.getMinAxisTickPixel(true)-xTickToLabelSpacing;

		StringAndPixel sp=directedDrawString(g,t,xp,yp,Compass.N);
		approxDrawString(g,sp.s,sp.x,sp.y);
	}

	public void drawYLAxisLabel(Graphics g, double y)
	{
		String t=getCoordText(yLScaler, y);
		int xp=xScaler.getMinAxisTickPixel(false)+yTickToLabelSpacing;
		int yp=yLScaler.pixel(y);

		StringAndPixel sp=directedDrawString(g,t,xp,yp,Compass.E);
		approxDrawString(g,sp.s,sp.x,sp.y);
	}
	
	public void drawYRAxisLabel(Graphics g, double y)
	{
		String t=getCoordText(yRScaler, y);
		int xp=xScaler.getMaxAxisTickPixel(false)-yTickToLabelSpacing;
		int yp=yRScaler.pixel(y);

		StringAndPixel sp=directedDrawString(g,t,xp,yp,Compass.W);
		approxDrawString(g,sp.s,sp.x,sp.y);
	}

	public void drawLabel(Graphics g, Vector labels, int compass)
	{
		
		int x,y;
		switch(compass)
		{
			case Compass.S:
			case Compass.C:
			case Compass.N:
				x=(xScaler.getMaxPixel()+xScaler.getMinPixel())/2;
				break;
			case Compass.W:
			case Compass.NW:
			case Compass.SW:
				x=xScaler.getMinPixel();
				break;
			case Compass.SE:
			case Compass.E:
			case Compass.NE:
				x=xScaler.getMaxPixel();
				break;
			default: return;
		}
		
		switch(compass)
		{
			case Compass.C:
			case Compass.W:
			case Compass.E:
				y=(yLScaler.getMaxPixel()+yLScaler.getMinPixel())/2;
				break;
			
			case Compass.NE:
			case Compass.NW:
			case Compass.N:
				y=yLScaler.getMaxPixel();
				break;
			case Compass.S:
			case Compass.SE:
			case Compass.SW:
				y=yLScaler.getMinPixel();
				break;
			default: return;
		}
		
		approxDrawStringsOnBox(g,labels,x,y,true);
	}
	

	/**
	 * Convenience class
	 */
	class StringAndPixel
	{
		String s;
		int x,y;
		StringAndPixel(String s, int x, int y)
		{
			this.s=s; this.x=x; this.y=y;
		}
	}
}
