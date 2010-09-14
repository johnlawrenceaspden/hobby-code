class TSDataSet  
{

    // member variables
    //
    
    /**
    * the name of the calendar for with which the data is interpreted
    */
    String calName
    
    /**
    * the calendar index for which the value at theData[0] corresponds.
    */
    public int          dataCalStartIndex; 
    
    /**
    * the actual data. the data at index n will be the data for calender
    * index (dataCalStartIndex + n)
    */
    public double[]	theData;
    
    // public methods
    //
    
    /**
    * return the range for which this data spans. eg:
    * calName = DC
    * dataCalStartIndex = 1000
    * theData has length 10
    *
    * getRange will return a TSXrange representing the min and max of
    * 1000,1009
    */
    public TSXRange getRange();
    
    // other methods depending on implementation chosen for the chart
    //
    
}
    
    
