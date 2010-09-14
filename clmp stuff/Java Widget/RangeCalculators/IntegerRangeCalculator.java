/**
 * A class to work out the range of a series of integers.
 * Create an instance, and then call include with all the numbers 
 * which need to be included in the range.
 * 
 * If the class is asked for a range when no numbers have been included 
 * it throws an exception.
 */
public class IntegerRangeCalculator
{
	boolean empty;
	int min, max;
	
	
	public IntegerRangeCalculator()
	{
		empty=true;
	}
	
	
	public void include(int x)
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
	
	public int getMin() throws EmptyRangeException
	{
		if(empty) throw new EmptyRangeException();
		else return min;
	}
	public int getMax() throws EmptyRangeException
	{
		if(empty) throw new EmptyRangeException();
		else return max;
	}
}
