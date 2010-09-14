import java.awt.*;
import java.util.*;

/**
 * A base class for more specialized Graph Panels.
 * Holds a UnitGraph and remembers legends and labels and
 * which grids are on and off.
 * It has placeholder functions to provide mouse labels and 
 * respond to selection events, which subclasses should override 
 */
public abstract class GraphPanel extends Panel 
	implements UnitGraphRangeSelectionListener, UnitGraphMouseLabelGenerator
{
	protected UnitGraphWithMouseSelect theGraph;
	private Vector theLabelArray;
	private UnitGraphLegendBox theLegendBox;
	private boolean lGridIsShowing, rGridIsShowing;
	
	public GraphPanel()
	{
		theLabelArray=new Vector();
		theLegendBox=new UnitGraphLegendBox(Compass.SW);
		theGraph=new UnitGraphWithMouseSelect();
		theGraph.addRangeSelectionListener(this);
		theGraph.setMouseLabelGenerator(this);
		lGridIsShowing=false;
		rGridIsShowing=false;
		this.setLayout(new CardLayout());
		this.add(theGraph,"");
	}
	
	public void addUnitGraphLabel(UnitGraphLabel a)
	{
		theLabelArray.addElement(a);	
	}
	
	public void addLegend(LineStyle l, String s)
	{
		theLegendBox.addLegend(l,s);
	}
	
	public void clearLegends()
	{
		theLegendBox=new UnitGraphLegendBox(Compass.SW);
	}

	/**
	 * Subclasses override this function in order to add elements 
	 * to the graph once the labels and legends have been placed
	 */
	protected abstract void showSubClassData();
	
	public void showData()
	{
		theGraph.clear();
		for(Enumeration e=theLabelArray.elements(); e.hasMoreElements();)
		{
			theGraph.addElement((UnitGraphLabel)e.nextElement());
		}
		if (theLegendBox!=null) theGraph.addElement(theLegendBox);
		showSubClassData();
		theGraph.remake();
	}
	
	/**
	 * Switch on/off the gridlines associated with the data on the right axis
	 */
	public void setRGrid(boolean on)
	{
		rGridIsShowing=on;
	}
	/**
	 * Switch on/off the gridlines associated with the data on the left axis
	 */
	public void setLGrid(boolean on)
	{
		lGridIsShowing=on;
	}
	protected boolean lGrid()
	{
		return lGridIsShowing;
	}
	protected boolean rGrid()
	{
		return rGridIsShowing;
	}
	
	/**
	 *Switch on the ability to draw a box with the mouse which fires a selection event
	 */
	public void enableSelection()
	{
	    theGraph.enableSelection();
	}
	
	/**
	 *Switch off the ability to draw a box with the mouse 
	 */
	public void disableSelection()
	{
	    theGraph.disableSelection();
	}

	/**
	 * Implementing UnitGraphRangeSelectionListener
	 */
	abstract public void RangeSelected(double p1, double p2, double p3, double p4);
	/**
	 * Implementing UnitGraphMouseLabelGenerator
	 */
	abstract public String getMouseLabel(double p1, double p2);
}
