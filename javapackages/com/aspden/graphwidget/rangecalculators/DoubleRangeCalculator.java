package com.aspden.graphwidget.rangecalculators;

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
	
        /** Create a range calculator which initially contains no points.
         */
	public DoubleRangeCalculator()
	{
		empty=true;	
	}
	
        /** Include a point in the range.
         * @param x The point to include.
         */
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
	
        /** get the low end of the range.
         * @throws EmptyRangeException if the range is empty. (<I>i.e.</I> if no points have been added since creation.
         * @return the lowest point included in the range.
         */
	public double getMin() throws EmptyRangeException
	{
		if(empty) throw new EmptyRangeException();
		else return min;
	}
        
        /** get the high end of the range.
         * @throws EmptyRangeException if the range is empty. (<I>i.e.</I> if no points have been added since creation.
         * @return the highest point included in the range.
         */
	public double getMax() throws EmptyRangeException
	{
		if(empty) throw new EmptyRangeException();
		else return max;
	}
}
