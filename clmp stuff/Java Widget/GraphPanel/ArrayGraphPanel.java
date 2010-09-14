import java.awt.*;
import java.util.*;

/**
 * This is the important object for display of multiple arrays.
 * Conceptually it is a collection of partial functions (DataArrays) which 
 * act as a GUI component
 * Each partial function is assigned to either the right or left axis.
 * Since this object knows all the data to be displayed, it can determine the ranges
 * on all three axes, and both create the axes and inform the partial functions of 
 * the complete dimensions of the graph so that they can scale themselves 
 * accordingly for display.
 */

public class ArrayGraphPanel extends GraphPanel
{
	private Vector theData;				//DataArrays to be displayed
	private Vector leftAxis;			//Whether the above are on the right or left
	private IntegerRangeCalculator xRange; //Range calculation objects
	private DoubleRangeCalculator yRRange,yLRange;
	private ArrayLabelGenerator theArrayLabelGenerator; //How to label the x-axis
	private Vector rangeListeners; //Array Range Selection Listeners
	
	public ArrayGraphPanel()
	{
		super();
		clearData();
		rangeListeners = new Vector();
	}
	
	/**
	 * Tell this object how it should label the points when it displays itself
	 */
	public void setArrayLabelGenerator(ArrayLabelGenerator a)
	{
		theArrayLabelGenerator=a;
	}
	
	/**
	 * Standard Listener model for range selection events.
	 */
	public void addRangeListener(ArrayRangeSelectionListener a)
	{
		rangeListeners.addElement(a);
	}
	
	/**
	 * reset the graph leaving listeners and labels attached
	 */
	public void clearData()
	{
		theData=new Vector();
		leftAxis=new Vector();
		xRange   = new IntegerRangeCalculator();
		yLRange  = new DoubleRangeCalculator();
		yRRange  = new DoubleRangeCalculator();
	}
	
	/**
	 * Add a new DataArray on the left axis
	 */
	public void addLData(DataArray da)
	{
		theData.addElement(da);
		leftAxis.addElement(new Boolean(true));
		da.includeValuesIn(yLRange);
		da.includeRangeIn(xRange);
	}
	
	/**
	 * Add a new DataArray on the right axis
	 */
	public void addRData(DataArray da)
	{
		theData.addElement(da);
		leftAxis.addElement(new Boolean(false));
		da.includeValuesIn(yRRange);
		da.includeRangeIn(xRange);
	}

	
	
	/**
	 * get the coordinate of i in [start, finish] rescaled to [0,1]
	 */
	private double xCoord(int i, int start, int finish)
	{
		if (start==finish) return 0.5;
		else return ((double)i-start)/(finish-start);	
	}
	/**
	 * Create an x-axis for the graph given the starting and finishing array indexes.
	 * For less that five points this should label all the points, for more it should 
	 * be an axis which labels itself evenly according to the size of the window.
	 */
	private UnitGraphAxis xAxis(int start, int finish, boolean gridlines)
	{
		if((finish-start)<5)
		{
			UnitGraphConstantAxis a=new UnitGraphConstantAxis(Compass.S, gridlines, true);
			for(int i=start; i<=finish; i++)
			{
				double x = xCoord(i, start, finish);
				a.addTick(x,1,getXAxisLabel(x));
			}
			return a;
		}
		else
		{
			UnitGraphAxisLabelGenerator b=new UnitGraphAxisLabelGenerator()
				{
					public String getLabel(double x)
					{
						return getXAxisLabel(x);
					}
					public String getMaximalLabel()
					{
						return theArrayLabelGenerator.getMaximalLabel();
					}
				};
			UnitGraphEvenLabelAxis a = new UnitGraphEvenLabelAxis(Compass.S, gridlines, b);
			return a;
		}
	}
	
	
	/**
	 * Gets all the DataArrays to convert themselves into unit graph representations
	 * and adds these and appropriate axes to ug. Boolean arguments determine whether 
	 * axes have attached gridlines or not.
	 * Note that this function relies on the RangeCalculator objects to contain the ranges
	 * of all the data. They throw EmptyRangeExceptions if they are asked for a range without
	 * having received data to range.
	 */
	protected void showSubClassData()
	{
		//add the axes, provided that there are ranges for them to represent.
		try{
			theGraph.addElement(xAxis(xRange.getMin(), xRange.getMax(), lGrid() || rGrid()));
		} catch(EmptyRangeException e){}
	
		try{
			UnitGraphAxis leftAxis=
				new UnitGraphFloatingPointAxis(Compass.W, lGrid(), yLRange.getMin(), yLRange.getMax());
			theGraph.addElement(leftAxis);
		} catch(EmptyRangeException e){}
		
		try{
			UnitGraphAxis rightAxis=
				new UnitGraphFloatingPointAxis(Compass.E, rGrid(), yRRange.getMin(), yRRange.getMax());
			theGraph.addElement(rightAxis);
		} catch(EmptyRangeException e){}
		
		/*Tell each DataArray the size of the whole graph and attach the UnitGraphObject it
		represents itself as to ug*/
		
		Enumeration de=theData.elements();
		Enumeration ae=leftAxis.elements();
		while( de.hasMoreElements())
		{
			DataArray a = (DataArray) de.nextElement();
			boolean   b = ((Boolean) ae.nextElement()).booleanValue();
			if(b)
			{
				try{
				theGraph.addElement(a.getUnitGraphPoints(xRange.getMin(), xRange.getMax(), yLRange.getMin(), yLRange.getMax()));
				} catch(EmptyRangeException e){}	//the only way this can happen (j'espere) is if all the data is NaN. If so we may as well not display it
			} 
			else
			{
				try{
				theGraph.addElement(a.getUnitGraphPoints(xRange.getMin(), xRange.getMax(), yRRange.getMin(), yRRange.getMax()));
				} catch(EmptyRangeException e){}
			}
		}
	}

	/**
	 * Given x,y in [0,1]x[0,1] get the appropriate Mouse Label.
	 * Here we're overriding the abstract function of GraphPanel 
	 * which implements the UnitGraphMouseLabelGenerator interface
	 * to provide concrete mouse labels
	 */
	public String getMouseLabel(double x,double y)
	{
		
		String label="";
		
		try{
			double ymin=yLRange.getMin();
			double ymax=yLRange.getMax();
			double yc=y*(ymax-ymin)+ymin;
		
			label=label + SpectralClass.getCoordText(ymax-ymin,yc) + " <- " ;
			
		} catch(EmptyRangeException e){}
		
		try{
			int xmin=xRange.getMin();
			int xmax=xRange.getMax();
			int xc=(int)Math.round(x*(xmax-xmin)+xmin);
			if(theArrayLabelGenerator!=null)
			{
				label = label + theArrayLabelGenerator.getLabel(xc); 
			}
		} catch(EmptyRangeException e){}
			
		try{
			double ymin=yRRange.getMin();
			double ymax=yRRange.getMax();
			double yc=y*(ymax-ymin)+ymin;
		
			label=label + " -> " + SpectralClass.getCoordText(ymax-ymin,yc)  ;
			
		} catch(EmptyRangeException e){}

		return label;
	}

	/**
	 * Given a coordinate in [0,1] get the associated x axis label
	 */
	public String getXAxisLabel(double x)
	{
		int xmin, xmax;
		try{
			xmin=xRange.getMin();
			xmax=xRange.getMax();
		}
		catch(EmptyRangeException e)
		{
			//No data so forget it.
			return null;
		}
		
		int xc=(int)Math.round(x*(xmax-xmin)+xmin);
		
		String label="";
		if(theArrayLabelGenerator!=null)
		{
			label = theArrayLabelGenerator.getLabel(xc); 
		}
		return label;
	}



	/**
	 * Not interested in the selected y range, so we convert the x coordinates to 
	 * array indexes and pass the message upwards
	 */
	public void RangeSelected(double x1, double y1, double x2, double y2)
	{
		try{
			int xmin=xRange.getMin();
			int xmax=xRange.getMax();
			int x1c=(int)Math.round(x1*(xmax-xmin)+xmin);
			int x2c=(int)Math.round(x2*(xmax-xmin)+xmin);
			
			Enumeration e=rangeListeners.elements();
			while(e.hasMoreElements())
			{
				((ArrayRangeSelectionListener)e.nextElement()).RangeSelected(x1c,x2c);
			}
		} catch(EmptyRangeException e){}
	}
}



	
	
	
	
	
	
	


