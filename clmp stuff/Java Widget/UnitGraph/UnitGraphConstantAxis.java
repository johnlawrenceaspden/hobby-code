import java.util.*;
import java.awt.*;

public class UnitGraphConstantAxis extends UnitGraphAxis
{
	private Vector coords, priorities, labels;
	
	public UnitGraphConstantAxis(int orientation, boolean gridlines)
	{
		this(orientation, gridlines, false);
	}
	
	public UnitGraphConstantAxis(int orientation, boolean gridlines, boolean leftlabels)
	{
		super(orientation, gridlines, leftlabels);
		coords = new Vector();
		priorities = new Vector();
		labels = new Vector();
	}
	
	public void addTick(double x, int priority, String label)
	{
		coords.addElement(new Double(x));	
		priorities.addElement(new Integer(priority));
		labels.addElement(label);
	}
	
	protected void doTicks(Graphics g, int pixels)
	{
		
		Enumeration ce=coords.elements();
		Enumeration pe=priorities.elements();
		Enumeration le=labels.elements();
		
		while(ce.hasMoreElements())
		{
			double x = ((Double) ce.nextElement()).doubleValue();
			int p = ((Integer) pe.nextElement()).intValue();
			String l = (String) le.nextElement();
			drawTick(g, x, p, l);
		}
	}
	

}
