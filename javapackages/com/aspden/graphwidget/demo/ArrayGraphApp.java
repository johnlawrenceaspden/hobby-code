package com.aspden.graphwidget.demo;

import com.aspden.graphwidget.graphpanel.array.*;
import com.aspden.graphwidget.misc.*;
import com.aspden.graphwidget.linestyles.*;
import com.aspden.graphwidget.unitgraph.*;
import com.aspden.graphwidget.unitgraph.objects.*;
import com.aspden.graphwidget.unitgraph.objects.axes.*;




import java.awt.*;
import java.awt.event.*;

/**
 * Test/demo application for the ArrayGraphPanel class
 */
public class ArrayGraphApp implements ItemListener, ArrayRangeSelectionListener
{
	private CloseableFrame theFrame;
	private ArrayGraphPanel thePanel;
	private Checkbox rGBox, lGBox;
	private Label theRangeLabel, thex1Label, thex2Label;
	private Panel theRangePanel;
	
	public ArrayGraphApp()
	{
		thePanel=new ArrayGraphPanel();
		thePanel.addRangeListener(this);
		addData();
		addLabels();
		
		Panel theButtonPanel=new Panel();
		rGBox=new Checkbox("Right Grid");
		lGBox=new Checkbox("Left Grid");
		theButtonPanel.add(lGBox);
		theButtonPanel.add(rGBox);
		rGBox.addItemListener(this);
		lGBox.addItemListener(this);
		
		theRangePanel=new Panel();
		
		theRangeLabel=new Label("No range selected");	
		thex1Label=new Label();
		thex2Label=new Label();
		theRangePanel.add(theRangeLabel);
		theRangePanel.add(thex1Label);
		theRangePanel.add(thex2Label);
		
		theFrame=new CloseableFrame("Array Graph");
		theFrame.setLayout(new BorderLayout());
		theFrame.add(theRangePanel, "North");
		theFrame.add(theButtonPanel,"South");
		theFrame.add(thePanel,"Center");
		theFrame.setSize(200,200);
		theFrame.show();

		thePanel.showData();
	}
	
	private void addData()
	{
		LineStyle l1, l2, l3;

		thePanel.setArrayLabelGenerator(new Weekday());
		
		if(true)
		{
			 l1=new BobblyLineStyle(Color.blue, Color.red);
			 l2=new BobblyLineStyle(Color.gray, Color.black);
			 l3=new BobblyLineStyle(Color.cyan, Color.blue);
		}
		else
		{
			 l1=new SimpleLineStyle(Color.red);
			 l2=new SimpleLineStyle(Color.black);
			 l3=new SimpleLineStyle(Color.blue);
		}
			
		thePanel.addRData(DataArray.getSample(1000,l1));
		thePanel.addLegend(l1, "1000 points");
		thePanel.addLData(DataArray.getSample(2000,l2));
		thePanel.addLegend(l2, "2000 points");
		thePanel.addRData(DataArray.getSample(3000,l3));
		thePanel.addLegend(l3, "3000 points");
		
	}
	
	private void addLabels()
	{
		thePanel.addUnitGraphLabel(new UnitGraphLabel(new Compass(Compass.C), "Central Label"));
		thePanel.addUnitGraphLabel(new UnitGraphLabel(new Compass(Compass.NW),"Northwesterly Label" ));
		{
			UnitGraphLabel l=new UnitGraphLabel(new Compass(Compass.E));
			l.addString("Easterly");
			l.addString("Label");
			l.addString("On three lines");
			thePanel.addUnitGraphLabel(l);
		}
	}
	
	public static void main(String[] args)
	{
		ArrayGraphApp a=new ArrayGraphApp();
	}

	private class Numberer implements ArrayLabelGenerator
	{
		public String getLabel(int i)
		{
			return (new Integer(i)).toString();
		}
		public String getMaximalLabel()
		{
			return ("999999");
		}
	}
	
	private class Weekday implements ArrayLabelGenerator
	{
		public String getLabel(int i)
		{
			String dayName=dayName(i);
			String day  = new Integer(i%365).toString();
			String year = new Integer(i/365).toString();
			
			return dayName+" the "+day+"th in the year of our Lord "+year;
		}
		
		public String getMaximalLabel()
		{
			return "wednesday the 365th in the year of our Lord 2000";
		}

		private String dayName(int i)
		{
			switch(i%7)
			{
			case 0: return  "Sunday";
			case 1: return "Monday";
			case 2: return "Tuesday";
			case 3: return "Wednesday";
			case 4: return "Thursday";
			case 5: return "Friday";
			case 6: return "Saturday";
			default: return "Oops";
			}
		}	
	}
	


	public void itemStateChanged(ItemEvent p1)
	{
		thePanel.setLGrid(lGBox.getState());		
		thePanel.setRGrid(rGBox.getState());	
		thePanel.showData();
	}

	
        public void rangeSelected(int a, int b)
	{
		Weekday w=new Weekday();
		theRangeLabel.setText(a+","+b);
		thex1Label=new Label(w.getLabel(a));
		thex2Label=new Label(w.getLabel(b));
		theRangePanel.invalidate();
	}
        
        public void rightButtonReleased()
        {
            //do nothing.
        }
}
