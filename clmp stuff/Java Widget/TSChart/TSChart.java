import java.awt.*;
import java.util.*;

/**
 * A GUI component to which one may add TSData (time series plots)
 * and which will display them in raw, expanded or collapsed form on request.
 * Range selection events are converted so that their co-ordinates are in 
 * terms of the original data.
 */
public class TSChart extends Panel implements ArrayRangeSelectionListener
{
	private ArrayGraphPanel thePanel; //the embedded array-displaying object
	private Vector theTSData;			//the list of TSData
	private ArrayLabelGenerator theArrayLabelGenerator; //Labels to be used on the graph
	private TSRangeTransformer theTSRangeTransformer; //Used to translate range selection events back into original coordinates
	private int expansionArray[]; private int expansionArrayStart; //to help translation to expanded scale
	private int collapseArray[];  private int collapseArrayStart;  //to help translation to collapsed scale
	private boolean boolCollapseArray[]; 
	
	private Vector theListeners;

	public TSChart()
	{
		thePanel=new ArrayGraphPanel();
		thePanel.addRangeListener(this);
		theTSData= new Vector();
		this.setLayout(new CardLayout());
		this.add(thePanel,"");
		//at the moment we don't want any translations performed on range selection events.
		theTSRangeTransformer=new NullTransformer();
		theListeners=new Vector();
	}
	
	/**
	 * Clear the graph.
	 */
	public void clearData()
	{
		theTSData=new Vector();
	}
	
	/**
	 * Conventional listener interface
	 */
	public void addArrayRangeSelectionListener(ArrayRangeSelectionListener a)
	{
		theListeners.addElement(a);
	}
	
	public void addTSData(TSData t)
	{
		theTSData.addElement(t);	
	}
	
	/**
	 * Add each TSData's name to the embedded array graph's legend box
	 */
	private void showLegends()
	{
		thePanel.clearLegends();
		for(Enumeration e = theTSData.elements();e.hasMoreElements();)
		{
			TSData t=(TSData) e.nextElement();
			thePanel.addLegend(t.style, t.name);
		}
	}
	
	/**
	 * Show the time series data as is. No weekends inserted, no missing data removed.
	 * A null operation.
	 */
	public void showRawData(ArrayLabelGenerator alg)
	{
		/**
		 * Since we're showing the data as is, there's no need to 
		 * transform the range given in a range selection event.
		 */
		theTSRangeTransformer=new NullTransformer();
		theArrayLabelGenerator=alg;
		thePanel.clearData();
		showLegends();
		
		Enumeration e = theTSData.elements();
		while(e.hasMoreElements())
		{
			TSData t=(TSData) e.nextElement();
			
			if(t.data.length==0) continue;
			DataArray da=new DataArray(t.start, t.data, t.style);
			
			if(t.left) thePanel.addLData(da); else thePanel.addRData(da);
		}
		
		thePanel.setArrayLabelGenerator(theArrayLabelGenerator);
	
		thePanel.showData();
	}
	
	/**
	 * Display the data after expanding the x-axis using the TSExpander.
	 * Labels are to be given by the ArrayLabelGenerator ACTING ON THE EXPANDED INDICES.
	 * This is because we need labels for points which do not exist in the unexpanded axis.
	 */
	public void showExpandedData(TSExpander tse, ArrayLabelGenerator alg)
	{
		showLegends();
		theArrayLabelGenerator = alg;
		thePanel.clearData();

		/**
		 *  For each partial function create a new one with the missing days 
		 *  added in and the function value there set to NaN
		 */
		for(Enumeration e = theTSData.elements();e.hasMoreElements();)
		{
			TSData t=(TSData) e.nextElement();
			
			if(t.data.length==0) continue;
			
			int finish=t.start+t.data.length-1;
			int newstart=tse.getNewIndex(t.start);
			int newfinish=tse.getNewIndex(finish);
			
			double d[]=new double[newfinish-newstart+1];
			
			//populate all entries with NaN.
			for(int i=0;i<newfinish-newstart+1;i++)d[i]=Double.NaN;
			//then put each element of the original array in its proper place.
			for(int i=0;i<t.data.length;i++)
			{
				int pos=t.start+i;
				int newpos=tse.getNewIndex(pos);
				int offset=newpos-newstart;
				d[offset]=t.data[i];
			}
			
			//add the new partial function to the graph.
			DataArray da=new DataArray(newstart, d, t.style);
			if(t.left) thePanel.addLData(da); else thePanel.addRData(da);
		}
		/*we need to know the total domain of the function collection
		in order to create the reverse translation table that allows
		range selection events to be expressed in terms of the original indexes*/
		IntegerRangeCalculator domain=new IntegerRangeCalculator();
		for(Enumeration e = theTSData.elements();e.hasMoreElements();)
		{
			TSData t=(TSData) e.nextElement();
			t.includeDomainInRangeCalculator(domain);
		}

		try{
			int min=domain.getMin();
			int max=domain.getMax();
			//create the translation table
			expansionArray=new int[max-min+1];
			expansionArrayStart=min;
			for(int i=0;i<max-min+1;i++) expansionArray[i]=tse.getNewIndex(i+min);
			//set the translation strategy to use the table.
			theTSRangeTransformer=new ExpandedTransformer();
		} catch (EmptyRangeException yikes){}
		
		thePanel.setArrayLabelGenerator(theArrayLabelGenerator);
		thePanel.showData();
	}

	public void showCollapsedData(ArrayLabelGenerator alg)
	{
		showLegends();
		theArrayLabelGenerator = alg;
		thePanel.clearData();

		/*we need to know the total domain of the function collection
		in order to create the collapse translation table*/
		IntegerRangeCalculator domain=new IntegerRangeCalculator();
		for(Enumeration e = theTSData.elements();e.hasMoreElements();)
		{
		    	System.out.println("count");
			TSData t=(TSData) e.nextElement();
			System.out.println("length="+t.data.length);
			if(t.data.length==0) continue;
			t.includeDomainInRangeCalculator(domain);
		}
		
		/**
		 * Create collapse translation table, which tells us which points on the x-axis are below valid data
		 * and what their indexes will be after collapse
		 */
		try{
			int min=domain.getMin();
			int max=domain.getMax();
			
			System.out.println("min="+min);
			System.out.println("max="+max);
			
			boolCollapseArray=new boolean[max-min+1];
			collapseArrayStart=min;
			for(int i=0;i<max-min+1;i++) boolCollapseArray[i]=false;
			
			/**
			 * Mark off the values in the collapse table over which there exists valid data
			 */
			for(Enumeration e = theTSData.elements();e.hasMoreElements();)
			{
				TSData t=(TSData) e.nextElement();
				
				for(int i=0;i<t.data.length;i++)
				{
					int pos=t.start+i-collapseArrayStart;
					if(!Double.isNaN(t.data[i])) boolCollapseArray[pos]=true;
				}
			}
			
			/**
			 * Create the collapse translation table like so:
			 * (-valid, x invalid)
			 * 
			 * - - x - - - x - - - - x
			 * 0 1   2 3 4   5 6 7 8  
			 * 
			 * Note how the new arrays always start from 0 !
			 */
			collapseArray=new int[max-min+1];
			int no_of_points=0;
			for(int i=0;i<max-min+1;i++)
			{
				if(boolCollapseArray[i]==true) 
				{
					collapseArray[i]=no_of_points++;
				}
			}
						
			/**
			 * Now for each data series we want to create a partial 
			 * function which covers the whole range.
			 * We could be much cleverer about this if it's too slow.
			 */
			for(Enumeration e = theTSData.elements();e.hasMoreElements();)
			{
				TSData t=(TSData) e.nextElement();
				
				double d[]=new double[no_of_points];
				//initial populate with 'no value'
				for(int i=0;i<d.length;i++) d[i]=Double.NaN;
				//put all the values from the original function into the new one
				for(int i=0;i<t.data.length;i++)
				{
					int pos=t.start+i;
					if(boolCollapseArray[pos-collapseArrayStart])
					{
					    int newpos=collapseArray[pos-collapseArrayStart];
					    d[newpos]=t.data[i];
					}
				}
				//add the new partial function to the graph.
				DataArray da=new DataArray(0, d, t.style);
				if(t.left) thePanel.addLData(da); else thePanel.addRData(da);
			}
			theTSRangeTransformer=new CollapseTransformer();
			thePanel.setArrayLabelGenerator(new CollapseLabelGenerator());
		} catch (EmptyRangeException yikes)
		{	//no data????! provide safe dummies.
			theTSRangeTransformer=new NullTransformer();
			thePanel.setArrayLabelGenerator(theArrayLabelGenerator);
		}
		thePanel.showData();
	}

	public void RangeSelected(int p1, int p2)
	{
		int a=theTSRangeTransformer.left(p1);
		int b=theTSRangeTransformer.right(p2);
		for(Enumeration e=theListeners.elements(); e.hasMoreElements();)
		{
			ArrayRangeSelectionListener l=(ArrayRangeSelectionListener)	e.nextElement();
			l.RangeSelected(a,b);
		}
	}

	/**
	 *The wise would implement a static reverse table here.
	 */
	private int translateCollapseCoordinate(int x) 
	{
		for(int i=0; i<collapseArray.length;i++)
		{
			if(collapseArray[i]>=x) return i+collapseArrayStart;
		}
		return collapseArray.length-1;
	}

	
	private final class CollapseLabelGenerator implements ArrayLabelGenerator
	{
		public String getMaximalLabel()
		{
			return theArrayLabelGenerator.getMaximalLabel();
		}
		public String getLabel(int p1)
		{
			return theArrayLabelGenerator.getLabel(translateCollapseCoordinate(p1));
		}
	}
	
	interface TSRangeTransformer
	{
		int left(int x);
		int right(int x);
	}
	
	private static final class NullTransformer implements TSRangeTransformer
	{
		public int left(int x) {return x;}
		public int right(int x){return x;}
	}
	
	private final class ExpandedTransformer implements TSRangeTransformer
	{
		public int left(int x) 
		{
			int i=expansionArray.length-1;
			while (i>0 && expansionArray[i]>x) i--;
			return expansionArrayStart+i;
		}
		public int right(int x)
		{
			int i=0;
			while (i<expansionArray.length-1 && expansionArray[i]<x) i++;
			return expansionArrayStart+i;
		}
	}

	private final class CollapseTransformer implements TSRangeTransformer
	{
		public int left(int x) 
		{
			return translateCollapseCoordinate(x);
		}
		public int right(int x)
		{
			return translateCollapseCoordinate(x);
		}
	}
	
	/**
	 * Pass-through functions which expose properties of the embedded graph
	 */
	public void addUnitGraphLabel(UnitGraphLabel a){thePanel.addUnitGraphLabel(a);}
	public void setLGrid(boolean b){thePanel.setLGrid(b);};
	public void setRGrid(boolean b){thePanel.setRGrid(b);};
	public void enableSelection(){thePanel.enableSelection();};
	public void disableSelection(){thePanel.disableSelection();};
}


