import java.util.*;

/**
 * An object which knows about a coordinate scale and a pixel scale and which
 * can convert between the two
 */

public class Scaler
{
	private int minPixel,maxPixel;
	private double minCoord, maxCoord;
	
	/**
	 * Converts pixel to coordinate value using the scaler
	 */
	public double coord(int i)
	{
		double s = (double)(i-minPixel)/(double)(maxPixel-minPixel);
		return s*(maxCoord-minCoord)+minCoord;
	}
	
	/**
	 * Converts coordinate to pixel value using the scaler
	 * Defended against integer wrap which occurs at small scales
	 */
	public int pixel(double x)
	{
		double s=(x-minCoord)/(maxCoord-minCoord);
		double pixel = (minPixel+s*(maxPixel-minPixel));
		if (pixel>10000) return 10000;
		if (pixel<-10000) return -10000;
		return (int) pixel;
		
	}
	
	public boolean isVisible(int x)
	{
		if(closestPixel(x)==x) return true;
		else return false;
	}
	

	private int closestPixel(int x)
	{
		if(minPixel<maxPixel)
		{
			if(x>maxPixel) x=maxPixel;
			if(x<minPixel) x=minPixel;
		}
		else
		{
			if(x<maxPixel) x=maxPixel;
			if(x>minPixel) x=minPixel;
		}
		return x;
	}
	
	public int getMinAxisPixel()
	{
		return minPixel;
	}
	
	public int getMinAxisTickPixel(boolean top)
	{
		int h=1;
		if(Math.abs(minPixel-maxPixel)>300) h=2;
		if(top) h=-h;
		return closestPixel(getMinAxisPixel()+h);
	}
	
	public int getMaxAxisPixel()
	{
		return maxPixel;
	}
	
	public int getMaxAxisTickPixel(boolean top)
	{
		int h=1;
		if(Math.abs(minPixel-maxPixel)>300) h=2;
		if(top) h=-h;
		return closestPixel(getMaxAxisPixel()+h);
	}

	
	/**
	 * Given a minimal gap (in pixels)
	 * works out an array of coordinates for gridlines/axis labels
	 */
	Vector cardinalPositions(int pixelGap)
	{
		//gap in pixels to a nice gap in coordinates
		double dx=pixelGap;
		dx *= (maxCoord-minCoord);
		dx /= (maxPixel-minPixel);
		if(dx<0) dx = -dx;
		dx = roundUp(dx);
		
		//find the least round value in the range
		double guess=minCoord/dx;
		guess=Math.floor(guess);
		
		double min=guess*dx;
		while (min > minCoord) min -=dx;
		while (min < minCoord) min +=dx;
		
		Vector v=new Vector();
		
		for(double x=min; x<=maxCoord; x+=dx) v.addElement(new Double(x));
		
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

	public void setMinPixel(int i){minPixel=i;}
	public void setMaxPixel(int i){maxPixel=i;}
	public void setMinCoord(double x){minCoord=x;}
	public void setMaxCoord(double x){maxCoord=x;}
	
	public double getMaxCoord(){return maxCoord;}
	public double getMinCoord(){return minCoord;}
	public int getMaxPixel(){return maxPixel;}
	public int getMinPixel(){return minPixel;}
	
}
