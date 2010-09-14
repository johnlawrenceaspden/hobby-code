import java.awt.*;
import java.util.*;

public class LabelHelper
{
	TextHelper theTextHelper;
	StyledLabel[] theLabels;
	
	public LabelHelper(TextHelper th)
	{
		theTextHelper=th;
		int i;
		
		theLabels=new StyledLabel[Compass.last+1];
		
		for(i=Compass.first; i<= Compass.last; i++)
		{
			theLabels[i]=new StyledLabel();
		}
	}
	
	public void AddLabel(String s, int compass)
	{
		theLabels[compass].strings.addElement(s);
	}
	
	public void RemoveLabels(int compass)
	{
		theLabels[compass].strings.removeAllElements();
	}
	
	public void setFont(Font f, int compass)
	{
		theLabels[compass].theFont=f;
	}
	
	public void setColour(Color c, int compass)
	{
		theLabels[compass].theColour=c;
	}
	
	public void addLabelsToImage(Image img)
	{
		Graphics g = img.getGraphics();
		for(int i=Compass.first; i<= Compass.last; i++)
		{
			Font oldFont=g.getFont();
			Color oldColour= g.getColor();
			if(theLabels[i].theFont!=null) g.setFont(theLabels[i].theFont);
			if(theLabels[i].theColour!=null) g.setColor(theLabels[i].theColour);
			
			theTextHelper.drawLabel(g,theLabels[i].strings,i);
			g.setFont(oldFont);
			g.setColor(oldColour);
		}
	}
	
	private class StyledLabel
	{
		Vector strings;
		Font theFont;
		Color theColour;
		
		public StyledLabel()
		{
			strings=new Vector();
			theFont=null;
			theColour=null;
		}
	}
}
