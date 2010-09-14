import java.awt.*;
import java.awt.event.*;
import java.util.*;

/**
 *A UnitGraph with the added ability to display the coordinates of the mouse pointer,
 *an optionally to allow click and drag selection of a range box, which it signals according to the 
 *usual event/listener model. 
 */
public class UnitGraphWithMouseSelect extends UnitGraph
{
	private UnitGraphMouseLabelGenerator theMouseLabelGenerator;
	private Vector rangeListeners = new Vector();
	
	
	private boolean selectionAllowed=true;

	private boolean pointerOverWindow=false;	
	
	private boolean mouseCoordsAreShowing=false;
	int mouseX;
	int mouseY;
	
	private boolean	selectionInProgress=false;
	int selectX;
	int selectY;
	

	protected void processMouseMotionEvent(MouseEvent e)
	{
		if(e.getID()==MouseEvent.MOUSE_MOVED)
		{
				//System.out.println("Mouse moved");
				pointerOverWindow=true;
				mouseCoordsAreShowing=true;
				mouseX=e.getX();
				mouseY=e.getY();
				repaint();
		}
 		if(e.getID()==MouseEvent.MOUSE_DRAGGED)
		{
				//System.out.println("Mouse dragged");
				mouseCoordsAreShowing=true;
				mouseX=e.getX();
				mouseY=e.getY();
				repaint();
		}
    		super.processMouseMotionEvent(e);
	}
	
	protected void processMouseEvent(MouseEvent e)
	{
		if(e.getID()==MouseEvent.MOUSE_EXITED)
		{
			//System.out.println("Mouse exit");
			pointerOverWindow=false;
			if(!selectionInProgress)
			{
		    	    mouseCoordsAreShowing=false;
			    repaint();
			}
		}
		if(e.getID()==MouseEvent.MOUSE_ENTERED)
		{
			//System.out.println("Mouse entry");
			pointerOverWindow=true;
			mouseCoordsAreShowing=true;

		}
		if(e.getID()==MouseEvent.MOUSE_PRESSED)
		{
			//System.out.println("Mouse pressed");
		    	mouseCoordsAreShowing=true;
			pointerOverWindow=true;
			if(selectionAllowed)
			{
			    selectionInProgress=true;
			    selectX=e.getX();
			    selectY=e.getY();
			    repaint();
			}
		}
		if(e.getID()==MouseEvent.MOUSE_RELEASED)
		{
			//System.out.println("Mouse released");
			if(selectionInProgress)
			{
				selectionInProgress=false;
				mouseCoordsAreShowing=false;
				repaint();
				fireRangeSelectionEvent(selectX,selectY,e.getX(),e.getY());
			}
		}

		super.processMouseEvent(e);
	}
	
	/**
	 *Attach an object to generate appropriate mouse labels given the mouse position on the 
	 *[0,1]x[0,1] scale.
	 */
	public void setMouseLabelGenerator(UnitGraphMouseLabelGenerator m)
	{
		this.theMouseLabelGenerator=m;
	}

	private void fireRangeSelectionEvent(int x1, int y1, int x2, int y2)
	{
		Enumeration e=rangeListeners.elements();
		while(e.hasMoreElements())
		{
			UnitGraphRangeSelectionListener rsl=(UnitGraphRangeSelectionListener) e.nextElement();
			rsl.RangeSelected(xCoord(x1),yCoord(y1), xCoord(x2), yCoord(y2));
		}
	}
	
	/**
	 *Usual event listener model
	 */
	public void addRangeSelectionListener(UnitGraphRangeSelectionListener r)
	{
		rangeListeners.addElement(r);
	}

	public UnitGraphWithMouseSelect()
	{
		enableEvents(AWTEvent.MOUSE_EVENT_MASK);
		enableEvents(AWTEvent.MOUSE_MOTION_EVENT_MASK);
	}
	
	/**
	 *Allow the user to click and drag a selection box which fires a range selection event when the
	 *mouse button is released.
	 */
	public void enableSelection()
	{
	    selectionAllowed=true;
	}
	
	/**
	 *Don't allow the user to draw the selection box.
	 */
	public void disableSelection()
	{
	    selectionAllowed=false;
	    selectionInProgress=false;
	    repaint();
	}
	
	private void drawMouseLabel(Graphics g, int pointerx, int pointery, int displayx, int displayy)
	{
		if(theMouseLabelGenerator!=null)
		{
			String s=theMouseLabelGenerator.getMouseLabel(xCoord(pointerx), yCoord(pointery));
			(new BoxObjectWrapper(new TextVector(s))).directedDraw(g,displayx,displayy,new Compass(Compass.SW));
		}
	}
	
	public void paint(Graphics g)
	{
		super.paint(g);
		if(mouseCoordsAreShowing)
		{
			if(selectionInProgress)
			{
				g.drawLine(selectX,selectY,selectX,mouseY);
				g.drawLine(selectX,mouseY,mouseX,mouseY);
				g.drawLine(mouseX,mouseY,mouseX,selectY);
				g.drawLine(mouseX,selectY,selectX,selectY);
				drawMouseLabel(g,selectX, selectY, selectX, selectY );
			}
			drawMouseLabel(g,mouseX,mouseY,getSize().width,0);
		}
	}
	

}
