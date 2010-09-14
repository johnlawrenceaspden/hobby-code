import java.awt.*;

/**
 * LineStyle derivatives draw styled lines on graphics contexts given sets of points.
 * the valid array determines whether a pixel is valid or not, and it is up to the line style to decide what to do
 * e.g. it could decide to break the line at that point, or to connect the points either side 
 */
abstract public class LineStyle
{
	abstract public void drawLine( Graphics g, int[] x, int[] y, boolean[] valid);

	/**
	 *Allows any subclass to draw a line segment without the calling 
	 *code assembling arrays.
	 */
	public void drawLine(Graphics g, int x1, int y1, int x2, int y2)
	{
	    int[] x=new int[2];
	    int[] y=new int[2];
	    boolean[] valid=new boolean[2];
	    
	    x[0]=x1; x[1]=x2;
	    y[0]=y1; y[1]=y2;
	    valid[0]=true; valid[1]=true;
	    
	    drawLine(g, x, y, valid);
	}
}
