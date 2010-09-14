import java.awt.*;

public class UnitGraphEvenLabelAxis extends UnitGraphAxisWithLabelGenerator
{
	
	UnitGraphEvenLabelAxis(int orientation, boolean gridlines, UnitGraphAxisLabelGenerator a)
	{
		super(orientation, gridlines, a, true);
	}
	
	protected void doTicks(Graphics g, int pixels)
	{
		String biggest=getMaximalLabel()+"   ";
		FontMetrics fm=g.getFontMetrics();
		int pixelgap;
		
		if(isVertical()) pixelgap=fm.getHeight()*2;
        else pixelgap=fm.stringWidth(biggest);
									 
		for(double x=1.0; x>=0.0; x-=((double)pixelgap)/pixels)
		{
			String label;
			drawTick(g,x,1,getLabel(x));		
		}
	}
}
