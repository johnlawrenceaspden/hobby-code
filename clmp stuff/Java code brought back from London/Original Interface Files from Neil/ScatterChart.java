interface ScatterChart
{

    /**
    * specifies an object which sould be notified when a range is selected
    * on the chart via a 'click-drag-release'. this object will implement the
    * ScatterChartListener Interface. Note that the chart may have more than one object
    * which wishes to be notified when a range is selected.
    */
    public void addScatterChartListener(ScatterChartListener listener);
    
    
    /**
    * sets the range of the x axis. 
    * no changes to display are made untill showData() is called
    */
    public void setScatterXRange(double xMin, double xMax);
    
    /**
    * returns the range of the graph as a ScatterXRange object (which
    * basically stores a double for xMin and an double for xMax). these ints
    * should correspond to the maximum in the case where we are displaying
    *
    */
    public ScatterXRange getScatterXRange();
    
    /**
    * sets the Dataseries which provides the data to be plotted on the
    * left axis.
    */
    public void setLeftAxisDataSource(VectorDataSeries dSeries);
    
    /**
    * sets the Dataseries which provides the data to be plotted on the
    * right axis.
    */
    public void setRightAxisDataSource(VectorDataSeries dSeries);
}
