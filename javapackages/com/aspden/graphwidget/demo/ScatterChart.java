package com.aspden.graphwidget.demo;

import com.aspden.graphwidget.graphpanel.scatter.*;
import com.aspden.graphwidget.linestyles.*;
import com.aspden.graphwidget.unitgraph.*;
import com.aspden.graphwidget.unitgraph.objects.*;

import java.awt.*;
import java.util.*;

public class ScatterChart extends Panel
{
	private ScatterGraphPanel thePanel; 
	private Vector theData;
	
	public ScatterChart()
	{
		thePanel=new ScatterGraphPanel();
		theData=new Vector();
		this.setLayout(new CardLayout());
		this.add(thePanel,"");
	}
	
	/**
	 * Clear the graph.
	 */
	public void clearData()
	{
		theData=new Vector();
	}
	
	public void addScatterData(ScatterData t)
	{
		theData.addElement(t);	
	}
	
	public void showData()
	{
		thePanel.clearLegends();
		thePanel.clearData();
		for(Enumeration e = theData.elements();e.hasMoreElements();)
		{
			ScatterData t=(ScatterData) e.nextElement();
			thePanel.addLegend(t.style, t.name);
			DataScatter d= new DataScatter(t.x, t.y, t.style);
			if(t.left) thePanel.addLData(d); else thePanel.addRData(d);
		}
		thePanel.showData();
	}
	/**
	 * Pass-through functions which expose properties of the embedded graph
	 */
	public void addUnitGraphLabel(UnitGraphLabel a){thePanel.addUnitGraphLabel(a);}
	public void setLGrid(boolean b){thePanel.setLGrid(b);}
	public void setRGrid(boolean b){thePanel.setRGrid(b);}
	public void setXRange(double min, double max) {thePanel.setXRange(min,max);}
	public void clearXRange(){thePanel.clearXRange();}
	public void enableSelection(){thePanel.enableSelection();}
	public void disableSelection(){thePanel.disableSelection();}

	public void addScatterRangeSelectionListener(ScatterRangeSelectionListener a)
	{
		thePanel.addRangeListener(a);
	}
}
