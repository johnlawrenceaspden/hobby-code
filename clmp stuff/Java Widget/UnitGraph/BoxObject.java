import java.awt.*;

/**
 * A BoxObject has a width and depth dependent on the graphics 
 * context, and can draw itself into a graphics context, 
 * given the point which is to be the top left corner
 */
public interface BoxObject
{
	public int getWidth(Graphics g);
	public int getDepth(Graphics g);
	public void draw(Graphics g, int x, int y);
}
