package com.aspden.graphwidget.demo;

import com.aspden.graphwidget.graphpanel.array.*;
import com.aspden.graphwidget.tschart.*;
import com.aspden.graphwidget.misc.*;
import com.aspden.graphwidget.unitgraph.*;
import com.aspden.graphwidget.linestyles.*;
import com.aspden.graphwidget.unitgraph.objects.*;
import com.aspden.graphwidget.unitgraph.objects.axes.*;


import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.text.*;

public class SampleTimeSeriesPanel extends Panel implements ArrayRangeSelectionListener, ItemListener, ActionListener
{
	ScalableTSChart chartA;
	ScalableTSChart chartB;
	ScalableTSChart chartC;

	Checkbox checkboxA;
	Checkbox checkboxB;
	Checkbox checkboxC;
	
	Panel theTSChartPanel;
	Panel theTSControlPanel;

	Label theRangeLabel;
	Button noRangeButton;
	
	Random theRandom = new Random();

	double aSeries[]=getFinancialLookingArray(1,0);
	double bSeries[]=getFinancialLookingArray(1.5,-0.75);
	double cSeries[]=getFinancialLookingArray(2,-1.0);
	
	public SampleTimeSeriesPanel()
	{
		chartA=new ScalableTSChart();
		chartB=new ScalableTSChart();
		chartC=new ScalableTSChart();
		
		chartA.addArrayRangeSelectionListener(this);
		chartB.addArrayRangeSelectionListener(this);
		chartC.addArrayRangeSelectionListener(this);
		
		
		theTSChartPanel=new Panel();
		
		theTSChartPanel.setLayout(new GridLayout(3,1));
		theTSChartPanel.add(chartA);
		theTSChartPanel.add(chartB);
		theTSChartPanel.add(chartC);
		
		chartA.addUnitGraphLabel(new UnitGraphLabel(new Compass(Compass.NW),"Raw Data"));
		chartB.addUnitGraphLabel(new UnitGraphLabel(new Compass(Compass.NW),"Expanded Data"));
		chartC.addUnitGraphLabel(new UnitGraphLabel(new Compass(Compass.NW),"Collapsed Data"));
		
		theTSControlPanel=new Panel();
		
		checkboxA=new Checkbox("doom", true);
		checkboxB=new Checkbox("terror", true);
		checkboxC=new Checkbox("fear", true);
		
		checkboxA.addItemListener(this);
		checkboxB.addItemListener(this);
		checkboxC.addItemListener(this);
		
		theRangeLabel=new Label("All Data Visible");
		noRangeButton = new Button("Show All Data");
		noRangeButton.addActionListener(this);
		
		theTSControlPanel.add(checkboxA);
		theTSControlPanel.add(checkboxB);
		theTSControlPanel.add(checkboxC);
		
		theTSControlPanel.add(theRangeLabel);
		theTSControlPanel.add(noRangeButton);
		
		
		this.setLayout(new BorderLayout());
		this.add(theTSChartPanel, "Center");
		this.add(theTSControlPanel, "South");
	}
	
	public void display()
	{
		chartA.clearData();
		chartB.clearData();
		chartC.clearData();
		
		addTSDataToChart(chartA);
		addTSDataToChart(chartB);
		addTSDataToChart(chartC);
		
		chartB.setLGrid(true);
		//chartC.setFont(new Font("Courier",Font.BOLD,10));
		//chartA.setBackground(Color.pink);
		//chartA.disableSelection();

		chartA.showRawData(new Weekday());
		chartB.showExpandedData(new WeekdayToDayExpander(), new Day());
		chartC.showCollapsedData(new Weekday());
		
	}
	private void addTSDataToChart(TSChart c)
	{
		LineStyle l;
		TSData theData;
		
		if(checkboxA.getState())
		{
			//l=new BobblyLineStyle(Color.black);
			l=new ThickLineStyle(Color.black, 4);
			theData = new TSData(aSeries, 0, l, "doom", true);
			c.addTSData(theData);
		}
		
		if(checkboxB.getState())
		{
			l=new BobblyLineStyle(Color.black, Color.red);
			theData = new TSData(bSeries, 0, l, "terror", false);
			c.addTSData(theData);
		}
		
		if(checkboxC.getState())
		{
			l=new BobblyLineStyle(Color.black, Color.orange);
			theData = new TSData(cSeries, 0, l, "fear", false);
			c.addTSData(theData);
		}
	}
	
	/**
	 * Implementing conventional ArrayRangeSelectionListener interface
	 */
	public void rangeSelected(int p1, int p2)
	{
		theRangeLabel.setText(p1+" "+p2);
		chartA.setRange(p1,p2);
		chartB.setRange(p1,p2);
		chartC.setRange(p1,p2);
		display();
	}
        
        
        
        public void rightButtonReleased()
        {
            //do nothing.
        }
	/**
	 * In our test calendars, 0 is Monday 1st January 2001.
	 * The Weekday calendar goes MTWTFMTWTF so that 9 is Friday 12th
	 * The Day calendar goes MTWTFSSMTWTFSS so that 11 is Friday 12th.
	 */
	static private class Weekday implements ArrayLabelGenerator
	{
		private static TSExpander wtod =new WeekdayToDayExpander();
		private static DateFormat df = DateFormat.getDateInstance(DateFormat.FULL);
  		public String getLabel(int x)
  		{
			Calendar c= new GregorianCalendar(2001, 0, wtod.getNewIndex(x)+1);
			Date d= c.getTime();
			return df.format(d);
  		}
			
  		public String getMaximalLabel()
  		{
			Calendar c= new GregorianCalendar(2001, 0, 3);
			Date d= c.getTime();
			return df.format(d);
  		}
  }
	
  /**
   * Day calendar. See Weekday.
   */
  private final static class Day implements ArrayLabelGenerator
  {
		private static DateFormat df = DateFormat.getDateInstance(DateFormat.FULL);
  		public String getLabel(int x)
  		{
  			String day;
			Calendar c= new GregorianCalendar(2001, 0, x+1);
			Date d= c.getTime();
			return df.format(d);
			
  		}
			
  		public String getMaximalLabel()
  		{
			Calendar c= new GregorianCalendar(2001, 8, 19); //Wednesday the 19th September 2001
			Date d= c.getTime();
			String day=df.format(d);
			return day;
  		}
	}
  
  /**
   * Day calendar. See Weekday.
   */
	private final static class WeekdayToDayExpander implements TSExpander
	{
		
		public int getNewIndex(int i)
		{
			int week = i/5;
			return i+(week*2);
		}
	}
	
	private double[] getFinancialLookingArray(double s,double t)
	{
		double a[]=new double[100];
		double val=1.0;
		boolean recording=true;
		
		for(int i=0;i<a.length;i++)
		{
			val+=s*theRandom.nextDouble()+t;
			if(theRandom.nextDouble()<0.05) recording=(!recording);
			if(recording) a[i]=val; else a[i]=Double.NaN;
		}
		return a;
	}



	public void itemStateChanged(ItemEvent p1)
	{
		display();
	}

	public void actionPerformed(ActionEvent p1)
	{
		theRangeLabel.setText("All Data Visible");
		chartA.clearRange();
		chartB.clearRange();
		chartC.clearRange();
		display();
	}	
}
