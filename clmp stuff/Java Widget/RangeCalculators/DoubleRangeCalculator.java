/**
 * A class to work out the range of a series of doubles.
 * Create an instance, and then call include with all the numbers 
 * which need to be included in the range.
 * NaNs are ignored!
 * 
 * If the class is asked for a range when no numbers have been included it throws 
 * an exception.
 */
public class DoubleRangeCalculator
{
	boolean empty;
	double min, max;
	
	public DoubleRangeCalculator()
	{
		empty=true;	
	}
	
	public void include(double x)
	{
		if(Double.isNaN(x)) return;
		
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
	
	public double getMin() throws EmptyRangeException
	{
		if(empty) throw new EmptyRangeException();
		else return min;
	}
	public double getMax() throws EmptyRangeException
	{
		if(empty) throw new EmptyRangeException();
		else return max;
	}
}
