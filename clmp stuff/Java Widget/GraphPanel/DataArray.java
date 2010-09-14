import java.util.*;
import java.awt.*;

/**
 * A function from a section of the integers to R union NaN.
 * NaN is to represent missing values.
 * It can include its values in Range calculation objects
 * and express itself as a UnitGraphPoints object.
 * It includes a LineStyle object which tells it how to draw itself.
 */
public class DataArray
{
	private int startIndex;
	private double[] y;
	private LineStyle theLineStyle;
	static DodgyRandom r=new DodgyRandom();

	public DataArray(int startindex, double y[], LineStyle l)
	{
		this.y = y;
		this.startIndex=startindex;
		this.theLineStyle=l;
	}
	
	/**
	 * Add the function values to a range calculator
	 */
	public void includeValuesIn(DoubleRangeCalculator r)
	{
		for(int i=0; i<y.length; i++)
		{
			r.include(y[i]);
		}
	}
	
	/**
	 * Add the function range to a range calculator
	 */
	public void includeRangeIn(IntegerRangeCalculator i)
	{
		i.include(startIndex);
		i.include(startIndex+y.length-1);
	}
	
	/**
	 * Given a scale for the whole graph, express the function as
	 * a UnitGraphPoints object.
	 */
	public UnitGraphPoints getUnitGraphPoints(int xmin, int xmax, double ymin, double ymax)
	{
		double xc[]=new double[y.length];
		double yc[]=new double[y.length];
		for(int i=0; i<y.length; i++)
		{
			if(Double.isNaN(y[i]))
			{
				xc[i]=yc[i]=Double.NaN;
			}
			else
			{
				xc[i]=((double)(i+startIndex-xmin))/(xmax-xmin);
				yc[i]=(y[i]-ymin)/(ymax-ymin);
			}
		}
		return new UnitGraphPoints(xc,yc,theLineStyle);
	}
	
	/**
	 * Test function providing a random instance
	 */
	public static DataArray getSample(int range, LineStyle style)
	{		
		if(range<1) throw new IllegalArgumentException();
		
		
		int start = r.nextInt(range);
		int len   = r.nextInt(range)+10;
		
		double x[]=new double[len];
		x[0]=10.0;
		for(int i=1;i<len;i++)
		{
			x[i]=x[i-1]+2*(r.nextDouble()-0.5);//len;
		}
		for(int i=0;i<len;i++)
		{
			if(r.nextInt(7)==0) x[i]=Double.NaN;
		}
		
		return new DataArray(start,x,style);
	}
}
