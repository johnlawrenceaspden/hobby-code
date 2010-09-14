interface Chart
{

    // the following 4 methods may well allready exist in
    // in the Canvas class.
    // 

  
    /**
    * set the font in which date labels and y values will be displayed in
    */
    public void setFont(Font font);
    
    /**
    * set the Background colour of the chart
    */
    public void setBackground(Color color);
    
    /**
    * get the Background colour of the chart
    */
    public Color getBackground();
    
    /**
    * set the Foreground colour of the chart (colour of axis and tickMarks)
    */
    public void setForeground(Color color);
    
    
    // methods for setting the data range
    //
    
    /**
    * if this is called with value true then chart should always
    * be plotted with the X Range equal to the X range of the longest graph
    * that it is plotting. otherwise range will be set manually.
    */
    public void setXMaxIsDefault(boolean set)
    
    // showing the data
    //
     
    /**
    * displays the data stored in the dataSources over the range last passed 
    * in setRange(int xMin, int xMax). if there is no data on the left then
    * do not show the left axis. similarly for right axis. if there is no data 
    * at all then do not show x axis.
    */
    public void showData();
    
    
    // labeling
    //
    
    /**
    * adds a label to the chart (e.g. "filter On"). the int tcb is an int
    * refering to whether the label should be positioned at the Top,Centre, or
    * Bottom of the graph. lcr refers similarly to Left, Centre, and Right
    * of chart. the labels final position should be xDisplacement pixels 
    * left of the 'corner' specified by tcb and lcr, and yDisplacement pixels 
    * above it.
    *
    * when the graph is resized the labels should still appear in accordance 
    * with these parameters.
    */ 
    public void  addChartLabel(  int tcb, 
    	    	    int lcr, 
    	    	    int xDisplacement,
		    int yDisplacement,
		    ArbLabel label,
		  );
		  
    /**
    * remove all labels from the graph.
    */
    public void removeAllChartLabels();
    
    // other state settings, which affect graphs behaviour
    //
    
    /**
    * if zoom is disabled then no rectangle is drawn when we 'click and drag'
    * on the chart. also we ensure that no chartListeners are informed when we 
    * 'click-drag-release' if we have disabled the zoom.
    */
    public void setRangeSelectionEnabled(boolean enabled);
    
    /**
    * if we pass display as true then we allways display the titles
    * of empty dataSets. otherwise we omit them from the legend.
    */
    public void displayTitlesForEmptyDataSets(boolean display); 
    
    /**
    * specifies whether a grid should be shown each time we graph something
    * or not.
    */
    public void setGridIsShowing(boolean gridOn);
    
    
  }
