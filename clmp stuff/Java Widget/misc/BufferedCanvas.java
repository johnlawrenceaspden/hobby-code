import java.awt.*;
import java.awt.event.*;

/**
 * A Canvas derivative which creates a background image which is redrawn
 * only when the size of the component changes.
 * Subclasses should override PaintImage in order to create this image and
 * override paint (whilst calling super.paint) in order to draw changeable
 * features over this image.
 * One can force a redraw using the remake function.
 */
public abstract class BufferedCanvas extends Canvas
{
	Image theImage;
	private boolean valid;
	
	public BufferedCanvas()
	{
		enableEvents(AWTEvent.COMPONENT_EVENT_MASK);
	}
	
	public void paint(Graphics g)
	{
		g.drawImage(theImage,0,0,this);
	}
	
	public void remake()
	{
		makeImage();
		repaint();
	}
	
	/**
	 * Overridden to prevent flickering
	 */
	public void update(Graphics g)
	{
		paint(g);
	}
	
	protected void paintImage(Graphics g)
	{
	}
	
	private void makeImage()
	{
			int w=getSize().width;
			int h=getSize().height;
			if(w>0 && h>0) theImage=createImage(w, h); else theImage=createImage(1,1);
			paintImage(theImage.getGraphics());
	}
	
	protected void processComponentEvent(ComponentEvent e)
	{
		if(e.getID()==ComponentEvent.COMPONENT_RESIZED
		   || e.getID()==ComponentEvent.COMPONENT_SHOWN)
		{
			makeImage();
		}
		super.processComponentEvent(e);
	}
}
