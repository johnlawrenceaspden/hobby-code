import java.awt.*;
import java.awt.event.*;
import java.util.*;


/**
 * Forms a net of communicating objects with a GraphHelper and a GridHelper and two Scalers
 * The Scalers hold the representation of the real coordinate rectangle and the 
 * on screen pixel rectangle and translate between the two, 
 * the Graph and Grid Helpers see the Scalers and use them to construct layers of the image.
 */
public class ScatterGraphComponent extends Canvas
{
	/**Conditional Compilation constant*/
	private static final boolean DEBUG_MESSAGES = false;
	
	//TODO: make this depend on font sizes
	/**minimal spacing in pixels of x and y axis points*/
	private static final int xAxisSpacing=40,yAxisSpacing=40;
	
	private GraphHelper theGraphHelper;
	private AxisGridHelper theAxisGridHelper;
	private Scaler xScaler, yLScaler, yRScaler;
	private TextHelper theTextHelper;
	private LabelHelper theLabelHelper;
	private LegendHelper theLegendHelper;
	
	/**Points to be plotted*/
	private Vector lData=new Vector();
	private Vector rData=new Vector();
	
	/**Buffered image in memory, over which mouse stuff can be drawn by paint.*/
	private Image theImage;
	
	private boolean graphIsShowing=false;
	private boolean lastPointsAreShowing=true;
	private boolean lGridIsShowing=false;
	private boolean rGridIsShowing=false;
	private boolean lAxisIsShowing=false;
	private boolean rAxisIsShowing=false;
	private boolean chooseOwnScale=false;
	
	private boolean mouseCoordsAreShowing=false;
	int mouseX;
	int mouseY;
	
	private boolean	selectionInProgress=false;
	int	selectX;
	int selectY;

	/**Assists in usual event/listener interface*/
	private Vector rangeListeners = new Vector();
	
	/**
	 * The range as instructed from outside.
	 */
	private double xRange1, xRange2;
	
	/**
	 * the actual range of the data to be displayed
	 */
	private double totalXRange1,totalXRange2;
	
	/**
	 * Set the range of the graph in real coordinates
	 * Will not allow the scale to become very small w.r.t the data
	 * since the maths for very distant points starts becoming inaccurate
	 */
	public void setRange(double x1,double x2) throws RangeTooSmallException
	{
		if(Math.abs(totalXRange1-totalXRange2)/Math.abs(x1-x2)>1000000) 
		{
			throw new RangeTooSmallException();
		}
		xRange1=x1;
		xRange2=x2;
	}
	

	/**
	 *Sets up the net of communicating objects which composes a Scatter Graph
	 */
	public ScatterGraphComponent()
	{
		enableEvents(AWTEvent.COMPONENT_EVENT_MASK);
		enableEvents(AWTEvent.MOUSE_EVENT_MASK);
		enableEvents(AWTEvent.MOUSE_MOTION_EVENT_MASK);
		
		xScaler=new Scaler();
		yLScaler=new Scaler();
		yRScaler=new Scaler();
		
		theTextHelper=new TextHelper();
		theTextHelper.setXScaler(xScaler);
		theTextHelper.setYLScaler(yLScaler);
		theTextHelper.setYRScaler(yRScaler);
		
		theGraphHelper = new GraphHelper();
		theGraphHelper.setLData(lData);
		theGraphHelper.setRData(rData);
		theGraphHelper.setXScaler(xScaler);
		theGraphHelper.setYLScaler(yLScaler);
		theGraphHelper.setYRScaler(yRScaler);
		theGraphHelper.setTextHelper(theTextHelper);
		
		theAxisGridHelper = new AxisGridHelper(xScaler, yLScaler, yRScaler, theTextHelper,xAxisSpacing,yAxisSpacing);
		theLabelHelper = new LabelHelper(theTextHelper);
		theLegendHelper = new LegendHelper(theTextHelper,xScaler,yLScaler,lData,rData);
		
	}
	
	
	
	/**
	 * When someone passes in some new data we range it
	 * for future reference.
	 */
	private void recalculateRange()
	{
		RangeCalculator r=new RangeCalculator(0,1);
		
		Enumeration e;
		e=lData.elements();
		while(e.hasMoreElements())
		{
			includePoints(r,(Points)e.nextElement());
		}
		e=rData.elements();
		while(e.hasMoreElements())
		{
			includePoints(r,(Points)e.nextElement());
		}
		
		totalXRange1=r.min; totalXRange2=r.max; 
	}
	private void includePoints(RangeCalculator r, Points p)
	{
		if(!isEmpty(p))
		{
			for(int i=0; i<p.getSize(); i++)
			{
				r.include(p.getX(i));
			}
		}
	}
	private boolean isEmpty(Points a)
	{
		if (a==null) return true;
		if (a.getSize()==0) return true;
		return false;
	}
	
	
	private boolean inRange(double x, double x1, double x2)
	{
		if(x1<=x && x<=x2) return true;
		if(x2<=x && x<=x1) return true;
		return false;
	}
	
	/**
	 * Find out the minimum and maximum y values over the given x range
	 */
	private Range GetDataYRange(double x1, double x2, Vector v)
	{
		RangeCalculator r=new RangeCalculator(0,1);
		Enumeration e=v.elements();
		while(e.hasMoreElements())
		{
			Points p=(Points)e.nextElement();
			if(!isEmpty(p))
			{
				for(int i=0; i<p.getSize(); i++)
				{
					if(inRange(p.getX(i),x1,x2)) {r.include(p.getY(i));}
				}
			}
		}
		
		return new Range(r.min,r.max);
	}
	
	
	/**
	 * Replot the graph onto the image in memory, 
	 * making all recent changes visible
	 */
	public void showData()
	{
		double x1,x2;
		
		if(chooseOwnScale)
		{
			x1=totalXRange1*1.1;
			x2=totalXRange2*1.1;
		}
		else
		{
			x1=xRange1;
			x2=xRange2;
		}
		
		xScaler.setMinCoord(x1);
		xScaler.setMaxCoord(x2);
		
		
		Range yLRange = GetDataYRange(x1,x2,lData);

		yLScaler.setMinCoord(yLRange.a);
		yLScaler.setMaxCoord(yLRange.b);
		
		Range yRRange = GetDataYRange(x1,x2,rData);

		yRScaler.setMinCoord(yRRange.a);
		yRScaler.setMaxCoord(yRRange.b);
	
		
		graphIsShowing=true;
		drawImage();
		repaint();
	}
	
	public void mousePaint(Graphics g)
	{
		if(mouseCoordsAreShowing)
		{
			if(selectionInProgress)
			{
				g.drawLine(selectX,selectY,selectX,mouseY);
				g.drawLine(selectX,mouseY,mouseX,mouseY);
				g.drawLine(mouseX,mouseY,mouseX,selectY);
				g.drawLine(mouseX,selectY,selectX,selectY);

				
				theTextHelper.putLabelAtCoords(g,selectX, selectY, lAxisIsShowing, rAxisIsShowing);
			}
		
			theTextHelper.putLabelAtCoords(g,mouseX, mouseY,lAxisIsShowing, rAxisIsShowing);
		}
	}
	
	public void paint(Graphics g)
	{
		if(graphIsShowing)
		{
			g.drawImage(theImage,0,0,this);
			mousePaint(g);
		}
	}
	
	private void drawImage()
	{
		if(!graphIsShowing) return;
		theImage=createImage(getSize().width, getSize().height);
		if(theImage==null) return;
			
		xScaler.setMinPixel(0+5);
		xScaler.setMaxPixel(getSize().width-5);
		yLScaler.setMinPixel(getSize().height-5);
		yLScaler.setMaxPixel(0+5);
		yRScaler.setMinPixel(getSize().height-5);
		yRScaler.setMaxPixel(0+5);
			
		if(lGridIsShowing)
		{
			theAxisGridHelper.addLGridToImage(theImage);
		}
		if(rGridIsShowing)
		{
			theAxisGridHelper.addRGridToImage(theImage);
		}
		theAxisGridHelper.addAxesToImage(theImage,lAxisIsShowing,rAxisIsShowing);
		theLegendHelper.addLegendsToImage(theImage);
		theLabelHelper.addLabelsToImage(theImage);
		theGraphHelper.addGraphToImage(theImage);
		if (lastPointsAreShowing)
		{
			theGraphHelper.addLastPointsToImage(theImage);
		}
	}
	
	protected void processComponentEvent(ComponentEvent e)
	{
		if(DEBUG_MESSAGES) System.out.println(e);
		if(e.getID()==ComponentEvent.COMPONENT_RESIZED)
		{
			mouseCoordsAreShowing=false;
			drawImage();
		}
		super.processComponentEvent(e);
	}

	protected void processMouseMotionEvent(MouseEvent e)
	{
		if(DEBUG_MESSAGES) System.out.println(e);
		if(e.getID()==MouseEvent.MOUSE_MOVED || e.getID()==MouseEvent.MOUSE_DRAGGED)
		{			mouseCoordsAreShowing=true;			mouseX=e.getX();			mouseY=e.getY();			repaint();
		}		super.processMouseMotionEvent(e);
	}
	
	protected void processMouseEvent(MouseEvent e)
	{
		if(DEBUG_MESSAGES) System.out.println(e);
		
		if(e.getID()==MouseEvent.MOUSE_EXITED)
		{
			mouseCoordsAreShowing=false;
			selectionInProgress=false;
			repaint();
		}
		if(e.getID()==MouseEvent.MOUSE_PRESSED)
		{
			mouseCoordsAreShowing=true;
			selectionInProgress=true;
			selectX=e.getX();
			selectY=e.getY();
			repaint();
		}
		if(e.getID()==MouseEvent.MOUSE_RELEASED)
		{
			mouseCoordsAreShowing=true;
			if(selectionInProgress)
			{
				selectionInProgress=false;
				repaint();
				fireRangeSelectionEvent(selectX,selectY,e.getX(),e.getY());
			}
		}

		super.processMouseEvent(e);
	}

	private void fireRangeSelectionEvent(int x1, int y1, int x2, int y2)
	{
		double xc1=xScaler.coord(x1);
		double xc2=xScaler.coord(x2);
		
		for(int i=rangeListeners.size(); i>0; i--)
		{
			RangeSelectionListener rsl;
			rsl=(RangeSelectionListener)rangeListeners.elementAt(i-1);
			rsl.RangeSelected(xc1,xc2);
		}
	}
	
	/*
	 * Get and Set Functions
	 */
	
	public void removeAllData()
	{
		rData.removeAllElements();
		lData.removeAllElements();
		recalculateRange();
		rAxisIsShowing=lAxisIsShowing=false;
	}
	
	public void addRData(Points p)
	{
		rData.addElement(p);
		recalculateRange();
		rAxisIsShowing=true;
	}

	public void addLData(Points p)
	{
		lData.addElement(p);
		recalculateRange();
		lAxisIsShowing=true;
	}

	
	public void setLGridIsShowing(boolean b)
	{
		lGridIsShowing=b;
	}
	public boolean getLGridIsShowing()
	{
		return lGridIsShowing;
	}
	public void setRGridIsShowing(boolean b)
	{
		rGridIsShowing=b;
	}
	public boolean getRGridIsShowing()
	{
		return rGridIsShowing;
	}
	
	/**
	 * Should the range be set by the SetRange function or by examining the data?
	 */
	public void setChooseOwnScale(boolean b)
	{
		chooseOwnScale=b;
	}
	public boolean getChooseOwnScale()
	{
		return chooseOwnScale;
	}
	
	public void setLastPointsAreShowing(boolean b)
	{
		lastPointsAreShowing=b;
	}
	public boolean getLastPointsAreShowing()
	{
		return lastPointsAreShowing;
	}

	/**
	 * Pass-through label functions
	 */
	public void addLabel(String s, int compass)
	{
		theLabelHelper.AddLabel(s,compass);
	}
	public void removeLabels(int compass)
	{
		theLabelHelper.RemoveLabels(compass);
	}
	public void setLabelFont(Font f, int compass)
	{
		theLabelHelper.setFont(f,compass);
	}
	public void setLabelColour(Color c, int compass)
	{
		theLabelHelper.setColour(c,compass);
	}

	
	/*Functions unlikely to be modified*/
	 
	/**
	 * Overridden to prevent flickering
	 */
	public void update(Graphics g)
	{
			paint(g);
	}
	
	/**
	 * Usual paradigm
	 */
	public void addRangeSelectionListener(RangeSelectionListener r)
	{
		rangeListeners.addElement(r);	
	}
	
	public Dimension getPreferredSize()
	{
		return new Dimension(800, 400);
	}

}
