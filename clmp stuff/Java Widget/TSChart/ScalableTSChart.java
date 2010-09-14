import java.util.*;
import java.awt.*;
/**
 * Wraps TSChart with the ability to display only a proportion of its
 * selected range.
 * As a side effect it also insulates the Chart against data in the data sets being changed
 * since it copies every value.
 */
public class ScalableTSChart extends TSChart
{
	private Vector theData=new Vector(); //A list of TSData
	private boolean rangeSet=false;		 //Has a range been set or should we just display everything?
	private int rangeA,rangeB;			 //range parameters if set
	Dimension prefSizeM = new Dimension(100,100);

	/**
	 * Add a TSData data series to the chart. 
	 * This includes information like the name, style and which 
	 * axis the chart's information is to be displayed on.
	 */
	public void addTSData(TSData t){theData.addElement(t);}
	
	

    public void setPreferredSize(int width,int height)
    {
	prefSizeM = new Dimension(width,height);
    }

    public Dimension getPreferredSize()
    {
	return prefSizeM;
    }
    public Dimension getMinimumSize()
    {
	return prefSizeM;
    }
    public Dimension getMaximumSize()
    {
	return prefSizeM;
    }
	/**
	 * When set, only data in the range a,b will be displayed
	 */
	public void setRange(int a, int b)
	{
		rangeSet=true;
		if(a<=b) 
		{
			this.rangeA=a;
			this.rangeB=b;
		}
		else
		{
			this.rangeA=b;
			this.rangeB=a;
		}
	}
	
	/**
	 * Forget any range which has been set and display all data.
	 */
	public void clearRange()
	{
		rangeSet=false;
	}
	
	/**
	 * Clear the chart.
	 */
	public void clearData()
	{
		theData=new Vector();
	}
	
	/**
	 * Takes the data as provided and strips and pads to fit the range.
	 * Copies the results to the subclass.
	 */
	private void copyData()
	{
		super.clearData();
		for(Enumeration e=theData.elements();e.hasMoreElements();)
		{
			TSData t= (TSData) e.nextElement();
			
			if(rangeSet)
			{
				//create an array with the required range
				double d[]=new double[rangeB-rangeA+1];
				//fill it with dummy values
				for(int i=0;i<=rangeB-rangeA;i++)
				{
					d[i]=Double.NaN;
				}
				
				//work out if there's an intersection with the real data
				int start=t.start;
				int finish=t.start+t.data.length-1;
				
				if(start<rangeA) start=rangeA;
				if(finish>rangeB) finish=rangeB;
			
				//copy that intersection over
				for(int i=start;i<=finish;i++)
				{
					d[i-rangeA]=t.data[i-t.start];
				}

				//bung the results to the subclass
				TSData n=new TSData(d, rangeA, t.style, t.name, t.left);
				super.addTSData(n);				
			}
			else
			{
				//just copy
				TSData n=new TSData((double[])t.data.clone(), t.start, t.style, t.name, t.left);
				super.addTSData(n);
			}
		}
	}
	
	public void showCollapsedData(ArrayLabelGenerator alg)
	{
		copyData();		super.showCollapsedData(alg);
	}

	public void showExpandedData(TSExpander tse, ArrayLabelGenerator alg)
	{
		copyData();		super.showExpandedData(tse, alg);
	}

	public void showRawData(ArrayLabelGenerator alg)
	{
		copyData();		super.showRawData(alg);
	}
}
