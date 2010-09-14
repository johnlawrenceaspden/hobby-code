package com.aspden.graphwidget.graphpanel.scatter;

import com.aspden.graphwidget.rangecalculators.*;
import com.aspden.graphwidget.misc.*;
import com.aspden.graphwidget.unitgraph.*;
import com.aspden.graphwidget.unitgraph.objects.*;
import com.aspden.graphwidget.unitgraph.objects.axes.*;
import com.aspden.graphwidget.graphpanel.*;

import java.util.*;

/**
 * An object to display multiple Scatter Graphs.
 * Each data series can be assigned to either right or left axis.
 */
public class ScatterGraphPanel extends GraphPanel
{
	private Vector theData;
	private Vector leftAxis;
	private Vector rangeListeners;
	
	/**
	 * These record whether an x range has been set externally or 
	 * whether it should be worked out from the data.
	 * The y-ranges are always computed from the data.
	 */
	private boolean xRangeSet; private double setXMin, setXMax;
	
	/**
	 * The ranges of the data as displayed for x, right y axis and left y axis.
	 * There's a boolean to say whether the ranges can be calculated (consider the case of a graph with no data)
	 */
	private boolean xRangeExists; private double xMax,xMin;
	private boolean yLRangeExists; private double yLMax,yLMin;
	private boolean yRRangeExists; private double yRMax,yRMin;
	
	public ScatterGraphPanel()
	{
		super();
		clearData();
		rangeListeners = new Vector();
	}
	
	public void setXRange(double min, double max)
	{
		if(max>=min)
		{
			setXMin=min;
			setXMax=max;
		}
		else
		{
			setXMin=max;
			setXMax=min;
		}
		xRangeSet=true;
	}
	
	public void clearXRange()
	{
		xRangeSet=false;
	}
	
	/**
	 * Conventional Listener model for range selection events
	 */
	public void addRangeListener(ScatterRangeSelectionListener a)
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
	}
	
	/**
	 * Add a new DataArray on the left axis
	 */
	public void addLData(DataScatter da)
	{
		theData.addElement(da);
		leftAxis.addElement(new Boolean(true));
	}
	
	/**
	 * Add a new DataArray on the right axis
	 */
	public void addRData(DataScatter da)
	{
		theData.addElement(da);
		leftAxis.addElement(new Boolean(false));
	}
	
	/**
	 * Works out the ranges for all the axes, storing them in global variables
	 */
	private void calculateRanges()
	{
		if(xRangeSet) //Either the x range has been set externally
		{
			xMax=setXMax;
			xMin=setXMin;
			xRangeExists=true;
		} //or we must work it out from the data
		else
		{
			DoubleRangeCalculator xRange=new DoubleRangeCalculator();
			Enumeration de=theData.elements();
			while(de.hasMoreElements())
			{
				DataScatter da = (DataScatter) de.nextElement();
				da.includeXValuesIn(xRange);
			}
			try{
				xMax=xRange.getMax();
				xMin=xRange.getMin();
				xRangeExists=true;
			}catch(EmptyRangeException e){xRangeExists=false;}
		}
		
		if(xRangeExists) //given that we now have a valid x-range
		{				 //work out the y ranges over that x range
			DoubleRangeCalculator yRRange=new DoubleRangeCalculator();
			DoubleRangeCalculator yLRange=new DoubleRangeCalculator();
			
			Enumeration de=theData.elements();
			Enumeration ae=leftAxis.elements();
		
			while( de.hasMoreElements())
			{
				DataScatter da = (DataScatter) de.nextElement();
				boolean   left = ((Boolean) ae.nextElement()).booleanValue();
				if(left)
				{
					da.includeYValuesIn(yLRange, xMin, xMax);
				}
				else
				{
					da.includeYValuesIn(yRRange, xMin, xMax);
				}
			}
		
			try{
				yRMax=yRRange.getMax();
				yRMin=yRRange.getMin();
				yRRangeExists=true;
			}catch(EmptyRangeException e){yRRangeExists=false;}
			try{
				yLMax=yLRange.getMax();
				yLMin=yLRange.getMin();
				yLRangeExists=true;
			}catch(EmptyRangeException e){yLRangeExists=false;}
		}
		else //no x range, which should mean an empty graph.
		{
				yRRangeExists=false;
				yLRangeExists=false;
		}
	}
	
	/**
	 *Add all the scatter graph data to the UnitGraph embedded in the superclass
	 */
	protected void showSubclassData()
	{
		calculateRanges();

		//add the axes, provided that there are ranges for them to represent.
		if(xRangeExists)  this.addElement(new UnitGraphFloatingPointAxis(new Compass(Compass.S), lGrid()||rGrid(), xMin, xMax));
		if(yLRangeExists) this.addElement(new UnitGraphFloatingPointAxis(new Compass(Compass.W), lGrid(), yLMin, yLMax));
		if(yRRangeExists) this.addElement(new UnitGraphFloatingPointAxis(new Compass(Compass.E), rGrid(), yRMin, yRMax));
		
		/*Tell each DataArray the size of the whole graph and attach the UnitGraphObject it
		represents itself as to the embedded UnitGraph*/
		
		Enumeration de=theData.elements();
		Enumeration ae=leftAxis.elements();
		while( de.hasMoreElements())
		{
			DataScatter a = (DataScatter) de.nextElement();
			boolean   b = ((Boolean) ae.nextElement()).booleanValue();
			if(b)
			{
				if(xRangeExists && yLRangeExists)
				{
					this.addElement(a.getUnitGraphPoints(xMin, xMax, yLMin, yLMax));
				}
				
			} 
			else
			{
				if(xRangeExists && yRRangeExists)
				{
					this.addElement(a.getUnitGraphPoints(xMin, xMax, yRMin, yRMax));
				}
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
		
		if(yLRangeExists)
		{
			double yc=y*(yLMax-yLMin)+yLMin;
                        
			label=label + new CoordinateFormatter(yLMax-yLMin).getCoordText(yc) + " <- " ;
		} 
		
		if(xRangeExists)
		{
			double xc=x*(xMax-xMin)+xMin;
			label=label + new CoordinateFormatter(xMax-xMin).getCoordText(xc);
		}
		
		if(yRRangeExists)
		{
			double yc=y*(yRMax-yRMin)+yRMin;
			label=label + " -> " + new CoordinateFormatter(yRMax-yRMin).getCoordText(yc);
		}
		
		return label;
	}
	
	/**
	 * Not interested in the selected y range, so we convert the x coordinates and pass the message upwards
	 */
	public void rangeSelected(double x1, double y1, double x2, double y2)
	{
		if(xRangeExists)
		{
			double x1c=x1*(xMax-xMin)+xMin;
			double x2c=x2*(xMax-xMin)+xMin;
			
			Enumeration e=rangeListeners.elements();
			while(e.hasMoreElements())
			{
				((ScatterRangeSelectionListener)e.nextElement()).rangeSelected(x1c,x2c);
			}
		} 
	}
        
	public void rightButtonReleased(double x, double y)
	{
            //do nothing.
	}
}
