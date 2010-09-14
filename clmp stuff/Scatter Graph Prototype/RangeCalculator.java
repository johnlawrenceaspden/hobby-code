/**
 * A class to work out the range of a series of doubles.
 * Create an instance, telling it what range it is to return as default if 
 * include is never called, and then call include with all the numbers 
 * which need to be included in the range.
 */
public class RangeCalculator
{
	boolean empty=true;
	double min, max;
	
	public RangeCalculator(double defaultMin, double defaultMax)
	{
		min=defaultMin;
		max=defaultMax;
	}
	
	public void include(double x)
	{
		if(empty)
		{
			empty=false;
			max=min=x;
		}
		else
		{
			if(x>max) max=x;
			if(x<min) min=x;
		}
	}
	
	public double getMin()
	{
		return min;
	}
	public double getMax()
	{
		return max;
	}
}
