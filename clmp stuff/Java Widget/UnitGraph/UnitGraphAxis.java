import java.awt.*;
import java.util.*;

/**
 * An axis at one edge of the unit square.
 * Orientation values can be Compass.N, E, S, W.
 */
public abstract class UnitGraphAxis extends UnitGraphObject
{
	private int orientation; 
	private boolean gridlines;
	private boolean leftlabels;
	
	private int width,height;
	private boolean drawingGrid;
	
	
	/**
	 * An axis can be on the North, South, East or West of the graph,
	 * can have associated gridlines or not, and can have its labels 
	 * displaced slightly to the left or placed exactly on the ticks
	 */
	public UnitGraphAxis(int orientation, boolean gridlines, boolean leftlabels)
	{
		if(!(orientation==Compass.N || orientation==Compass.E || orientation==Compass.S || orientation==Compass.W))
		{
			throw new IllegalArgumentException();	
		}
		this.orientation=orientation;
		this.gridlines=gridlines;
		this.leftlabels=leftlabels;
	}
	
	public UnitGraphAxis(int orientation, boolean gridlines)
	{
		this(orientation, gridlines, false);
	}
	

	private int getPixelWidthOfAxis()
	{
		if(isVertical())
		{
			return height;	
		}
		else
		{
			return width;	
		}
	}
	
	/**
	 * given the x&y coordinates of the base of the tick, top of the tick, and top of the gridline
	 * p0 p1                                     p2 
	 * ===---------------------------------------
	 * and the label and which direction it is to be drawn relative to the top of the tick,
	 * draw the 
	 */
	private void drawTick(Graphics g, int x0, int y0, int x1, int y1, int x2, int y2, String label, Compass offset)
	{
		if(gridlines && drawingGrid)
		{
			Color c=g.getColor();
			g.setColor(Color.lightGray);
			g.drawLine(x0,y0,x2,y2);
			g.setColor(c);
		}
		if(!drawingGrid)
		{
			g.drawLine(x0,y0,x1,y1);
			(new BoxObjectWrapper(new TextVector(label))).directedDraw(g,x1,y1,offset);
		}
	}
	
	/**
	 * Subclasses should call this function from the doTicks function which they implement.
	 * It relies on width height and drawingGrid having already been set appropriately by
	 * the draw or drawGrid functions which call doTicks.
	 * A sample call might draw a tick labelled "My Tick" at 0.7 of the way along the axis
	 * with priority 1, meaning that it is the biggest sort of tick.
	 */
	protected void drawTick(Graphics g, double x, int priority, String label)
	{
		int x0,y0,x1,y1,x2,y2;
		Compass offset;
		int ticklen=2/priority+1;
		
		
		switch(orientation)
		{
		case Compass.N:
		case Compass.S:
			x0=x1=x2=(int)(width*x);
			if(orientation==Compass.N)
			{
				y0=0;
				y1=ticklen;	
				y2=height;
				if(leftlabels) offset=new Compass(Compass.SW);
				else offset=new Compass(Compass.S);
			}
			else
			{
				y0=height;
				y1=height-ticklen;
				y2=0;
				if(leftlabels) offset=new Compass(Compass.NW);
				else offset=new Compass(Compass.N);
			}
			break;
		default:
			y0=y1=y2=(int)(height*(1-x));
			
			{	//don't want EW axes interfering with NS axes, so there's a 
				//band in which we shouldn't put ticks.
				int band = 3*((g.getFontMetrics()).getHeight())/2;
				if(y0<band || y0>(height-band)) return;
			}
			if(orientation==Compass.W)
			{
				x0=0;
				x1=ticklen;
				x2=width;
				offset=new Compass(Compass.E);
			}
			else
			{
				x0=width;
				x1=width-ticklen;	
				x2=0;
				offset=new Compass(Compass.W);
			}
		}
		drawTick(g,x0,y0,x1,y1,x2,y2, label, offset);
	}

	/**
	 * Subclasses should override this with a function that calls 
	 * the protected function drawTick(g, x, priority, label)
	 * for each x at which a tick is to be placed.
	 */
	protected abstract void doTicks(Graphics g, int pixels);

	/**
	 * Draws the gridlines without the ticks.
	 * This is so that the containing graph can make sure that one axis'
	 * gridlines don't cover another one's ticks.
	 */
	public final void drawGrid(Graphics g, int w, int h)
	{
		drawingGrid=true;
		this.width=w;
		this.height=h;
		this.doTicks(g,getPixelWidthOfAxis());
	}
	
	/**
	 * Draws the ticks without the grid lines.
	 * This is so that the containing graph can make sure that one axis'
	 * gridlines don't cover another one's ticks.
	 */
	public final void draw(Graphics g, int w, int h)
	{
		drawingGrid=false;
		this.width=w;
		this.height=h;
		this.doTicks(g,getPixelWidthOfAxis());
	}
	
	protected boolean isVertical()
	{
		if (orientation==Compass.W || orientation==Compass.E)
		{
			return true;
		}
		else return false;
	}
}
