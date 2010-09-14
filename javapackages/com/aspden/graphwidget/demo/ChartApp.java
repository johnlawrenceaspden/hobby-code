package com.aspden.graphwidget.demo;

import com.aspden.graphwidget.misc.*;


import java.awt.*;
import java.util.*;

public class ChartApp
{
	CloseableFrame theFrame;
	
	public ChartApp()
	{
		theFrame=new CloseableFrame("ChartApp");
		
		SampleTimeSeriesPanel theTSPanel=new SampleTimeSeriesPanel();
		//SampleScatterPanel theScatterPanel=new SampleScatterPanel();
		
		theFrame.setLayout(new GridLayout());
		theFrame.setBackground(Color.white);
		theFrame.add(theTSPanel);
		//theFrame.add(theScatterPanel);
		theFrame.setSize(600,600);
		theFrame.show();
		
		//Have to do this manually since the double-buffering thing won't work until 
		//there is a panel in existence. Any ideas?
		theTSPanel.display();
		//theScatterPanel.display();
	}

	public static void main(String[] args)
	{
		new ChartApp();
	}
}
