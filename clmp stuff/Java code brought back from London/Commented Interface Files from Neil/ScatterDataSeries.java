 import java.util.*;
/**
 * Should be a collection of StyledScatterDataSets
 */

public class ScatterDataSeries
{
    
    // member variables
    //
    
    /**
	 * this is the group of the StyledScatterDataSets to be displayed upon the graph
	 * @SBGen Collection of StyledScatterDataSet
	 */
	private Vector theStyledDataSets = new Vector();
  
    
    // methods
    //
   
    /**
    * adds a styled dataSet at the specified index in theStyledDataSets
    * note that it may have to extend the size of theStyledDataSets to
    * do this.
    */
    public void setDataSet(int index,StyledScatterDataSet ds)
    {
    	
    }
    
    /**
    * adds a styled dataSet to the end of theStyledDataSets
    */
    public void addDataSet(StyledScatterDataSet ds)
    {
    	
    }
    
    /**
    * Remove all the styles DataSets from this series
    */
    public void removeAllDataSets()
    {
	
    }
    
    /**
    * Check if there is at least one DataSet in this series.
    * Need to check the contents of "theStyledDataSets" as it may not be empty
    * but all null
    */
    public boolean isEmpty()
    {
		return true;
    }
    
}	
