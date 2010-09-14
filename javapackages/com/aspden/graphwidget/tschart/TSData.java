package com.aspden.graphwidget.tschart;

import com.aspden.graphwidget.linestyles.*;
import com.aspden.graphwidget.rangecalculators.*;


/**
 * Some Time Series data. Has a start position, some data, a linestyle, 
 * a name, and knows whether it wants to be on the left or the right axis.
 */
public class TSData
{
	double data[];
	int start;
	LineStyle style;
	String name;
	boolean left;
	
	public TSData(double a[], int start, LineStyle s, String name, boolean left)
	{
		this.data=a;
		this.start=start;
		this.style=s;
		this.name=name;
		this.left=left;
	}
	
	/**
	 * for whe we need to work out the appropriate domain for a collection of these functions
	 */
	public void includeDomainInRangeCalculator(IntegerRangeCalculator i)
	{
		i.include(start);
		if(data.length>0) i.include(start+data.length-1);
	}
}
