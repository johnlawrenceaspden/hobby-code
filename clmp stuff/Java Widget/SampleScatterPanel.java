import java.awt.*;
import java.util.*;
import java.awt.event.*;

public class SampleScatterPanel extends Panel implements ScatterRangeSelectionListener, ItemListener, ActionListener
{
	ScatterChart chartS;
	Panel controlPanel;
	Random theRandom=new Random();
	
	private final int no=4;
	
	LineStyle[] styles= new LineStyle[]{
						   new BobblyLineStyle(Color.blue, Color.green), 
						   new BobblyLineStyle(Color.gray, Color.cyan),
						   new BobblyLineStyle(Color.magenta, Color.pink),
						   new BobblyLineStyle(Color.orange, Color.darkGray)};
	String[] names=new String[]{"frogs", "boils", "locusts", "mobiles"};
	
	double[][] x=new double[no][];
	double[][] y=new double[no][];
	
	CheckboxGroup[] cbg=new CheckboxGroup[no];
	Checkbox[] lcb=new Checkbox[no];
	Checkbox[] rcb=new Checkbox[no];
	
	public SampleScatterPanel()
	{
		for(int i=0;i<no;i++){
			x[i]=getArray(100,1,-0.3);
			y[i]=getArray(100,2,-1);
			cbg[i]=new CheckboxGroup();
			lcb[i]=new Checkbox(names[i],cbg[i],false);
			rcb[i]=new Checkbox("",cbg[i],true);
		}
		
		chartS=new ScatterChart();
		controlPanel=new Panel();
		controlPanel.setLayout(new FlowLayout());
		
		Button allDataButton=new Button("Show All Data");
		controlPanel.add(allDataButton);
		allDataButton.addActionListener(this);

		for(int i=0;i<no;i++){
			controlPanel.add(lcb[i]);
			controlPanel.add(rcb[i]);
			lcb[i].addItemListener(this);
			rcb[i].addItemListener(this);
		}
		
		chartS.addScatterRangeSelectionListener(this);
		this.setLayout(new BorderLayout());
		this.add(chartS, "Center");
		this.add(controlPanel,"South");
	}
	
	public void display()
	{
		chartS.clearData();
		for(int i=0;i<no;i++) chartS.addScatterData(new ScatterData(x[i],y[i],styles[i],names[i], lcb[i].getState()));
		UnitGraphLabel lbl=new UnitGraphLabel(Compass.NW,"Scatter Data");
		lbl.addString("Double values on both axes");
		chartS.addUnitGraphLabel(lbl);
		chartS.showData();
	}
		
	/**
	 * Implementing ScatterRangeSelectionListener interface
	 */
	public void RangeSelected(double a, double b)
	{
		chartS.setXRange(a,b);
		chartS.showData();
	}
	
	private double[] getArray(int n, double m, double b)
	{
		double a[]=new double[n];
		double val=0.0;
		for(int i=0;i<a.length;i++)
		{
			val+=m*theRandom.nextDouble()+b;
			a[i]=val;
		}
		return a;
	}

	public void itemStateChanged(ItemEvent p1)
	{
		display();
	}

	public void actionPerformed(ActionEvent p1)
	{
		chartS.clearXRange();
		chartS.showData();
	}
}
