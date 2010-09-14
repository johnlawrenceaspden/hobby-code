package com.aspden.graphwidget.demo;

import com.aspden.graphwidget.graphpanel.scatter.*;
import com.aspden.graphwidget.misc.*;
import com.aspden.graphwidget.linestyles.*;
import com.aspden.graphwidget.unitgraph.*;
import com.aspden.graphwidget.unitgraph.objects.*;
import com.aspden.graphwidget.unitgraph.objects.axes.*;

import java.awt.*;
import java.awt.event.*;

/**
 * Test/Demo for Scatter Graphs.
 */
public class ScatterGraphApp implements ItemListener, ScatterRangeSelectionListener, ActionListener
{
	private CloseableFrame theFrame;
	private ScatterGraphPanel thePanel;
	private Checkbox rGBox, lGBox;
	private Label theRangeLabel;
	private Panel theRangePanel;
	private Button noRangeButton;
	
	public ScatterGraphApp()
	{
		thePanel=new ScatterGraphPanel();
		thePanel.addRangeListener(this);
		addData();
		addLabels();
		
		Panel theButtonPanel=new Panel();
		rGBox=new Checkbox("Right Grid");
		lGBox=new Checkbox("Left Grid");
		noRangeButton=new Button("Show all Data");
		theButtonPanel.add(lGBox);
		theButtonPanel.add(rGBox);
		theButtonPanel.add(noRangeButton);
		noRangeButton.addActionListener(this);
		rGBox.addItemListener(this);
		lGBox.addItemListener(this);
		
		theRangePanel=new Panel();
		
		theRangeLabel=new Label("No range selected");	
		theRangePanel.add(theRangeLabel);
		
		theFrame=new CloseableFrame("ScatterGraphApp");
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
			
		thePanel.addRData(getRandomScatterSample(100,l1));
		thePanel.addLegend(l1, "100 points");
		thePanel.addLData(getRandomScatterSample(200,l2));
		thePanel.addLegend(l2, "200 points");
		thePanel.addRData(getRandomScatterSample(300,l3));
		thePanel.addLegend(l3, "300 points");
		
	}
	
	private DataScatter getRandomScatterSample(int n, LineStyle l)
	{
		double x[]=new double[n];
		double y[]=new double[n];
		x[0]=y[0]=0.0;
		for(int i=1;i<n;i++)
		{
			x[i]=x[i-1]+Math.random();
			y[i]=y[i-1]+Math.random()-0.5;
		}
		return new DataScatter(x,y,l);
	}
	
	
	private void addLabels()
	{
		thePanel.addUnitGraphLabel(new UnitGraphLabel(new Compass(Compass.C), "Central Label"));
		thePanel.addUnitGraphLabel(new UnitGraphLabel(new Compass(Compass.NW),"Northwesterly Label" ));
		{
			UnitGraphLabel l=new UnitGraphLabel(new Compass(Compass.E));
			l.addString("Revolutionary Easterly");
			l.addString("Label");
			l.addString("On three lines");
			thePanel.addUnitGraphLabel(l);
		}
	}
	
	public static void main(String[] args)
	{
		ScatterGraphApp a=new ScatterGraphApp();
	}

	public void itemStateChanged(ItemEvent p1)
	{
		thePanel.setLGrid(lGBox.getState());		
		thePanel.setRGrid(rGBox.getState());	
		thePanel.showData();
	}

	public void rangeSelected(double a, double b)
	{
		theRangeLabel.setText(a+","+b);
		thePanel.setXRange(a,b);
		thePanel.showData();
	}

	public void actionPerformed(ActionEvent p1)
	{
		theRangeLabel.setText("No Range Selected");
		thePanel.clearXRange();
		thePanel.showData();
	}
}

