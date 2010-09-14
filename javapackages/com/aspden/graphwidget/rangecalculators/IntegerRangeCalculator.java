package com.aspden.graphwidget.rangecalculators;

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
	
	
        /** Create a range calculator which initially contains no points.
         */
	public IntegerRangeCalculator()
	{
		empty=true;
	}
	
	
        /** Include a point in the range.
         * @param x The point to include.
         */
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
	
        /** get the low end of the range.
         * @throws EmptyRangeException if the range is empty. (<I>i.e.</I> if no points have been added since creation.
         * @return the lowest point included in the range.
         */
	public int getMin() throws EmptyRangeException
	{
		if(empty) throw new EmptyRangeException();
		else return min;
	}
        
        /** get the high end of the range.
         * @throws EmptyRangeException if the range is empty. (<I>i.e.</I> if no points have been added since creation.
         * @return the highest point included in the range.
         */
	public int getMax() throws EmptyRangeException
	{
		if(empty) throw new EmptyRangeException();
		else return max;
	}
}
