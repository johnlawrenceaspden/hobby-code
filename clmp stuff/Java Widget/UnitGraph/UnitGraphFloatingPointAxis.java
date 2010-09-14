import java.awt.*;
import java.util.*;
import java.text.*;


/**
 * An axis which will represent a certain floating point range and label itself 
 * according to the actual screen size
 */
public class UnitGraphFloatingPointAxis extends UnitGraphAxis
{
	private double a,b;
		
	public UnitGraphFloatingPointAxis( int orientation, boolean gridlines, double a, double b)
	{
		super(orientation, gridlines);
		this.a=a;
		this.b=b;
	}

	/**
	 * Plot axis ticks depending on graphical situation
	 */
	protected void doTicks(Graphics g, int pixels)
	{
		//How many pixels does the biggest label and some space take up?
		String biggest=SpectralClass.getMaximalCoordText(b-a)+"   ";
		FontMetrics fm=g.getFontMetrics();
		int pixelGap;
		
		if(isVertical()) pixelGap=fm.getHeight()*2;
        else pixelGap=fm.stringWidth(biggest);
		
		//Given the total length in pixels and the size of the largest possible label, 
		//get a load of round numbers which can be used as axis ticks without overlapping.
		Enumeration e;
		e=cardinalPositions(pixelGap, pixels).elements();	
		
		//Then use the superclass function to plot them all.
		while(e.hasMoreElements()) 
		{
			double x=((Double)e.nextElement()).doubleValue();
			drawTick(g,(x-a)/(b-a),1,SpectralClass.getCoordText(b-a,x));	
		}
	}
	
	
	
	/**
	 * Gets a selection of round numbers in a range.
	 */
	private Vector cardinalPositions(int pixelGap, int pixels )
	{
		//gap in pixels to a nice gap in coordinates
		double dx=pixelGap;
		dx *= (b-a);
		dx /= (pixels);
		if(dx<0) dx = -dx;
		dx = roundUp(dx);
		
		//find the least round value in the range
		double guess=a/dx;
		guess=Math.floor(guess);
		
		double min=guess*dx;
		while (min > a) min -=dx;
		while (min < a) min +=dx;
		
		Vector v=new Vector();
		if(b>a)	for(double x=min; x<=b; x+=dx) v.addElement(new Double(x));
		else for(double x=min; x>=b; x-=dx) v.addElement(new Double(x));
		
		return v;
	}
	
	/**
	 * Rounds up to numbers like 100, 50, 25, 12.5, 10, 5, 2.5, 1.25, 1, 0.5, 0.25 
	 */
  	private double roundUp(double x)
	{
		//find the power of 10 just above x
		double r=1.0;
		while (r>x) r/=10;
		while (r<x) r *=10;
		
		//keep halving it, but don't let it go below x
		while (r/2>x) r/=2;
		return r;
	}
}
