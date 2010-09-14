import java.awt.Color;

/**
 * Convenience class holding an array of (x,y) pairs
 * and some styling info
 */
public class Points
{
	private Color colour;
	private double xcoords[];
	private double ycoords[];
	private int size;
	private String legend;
	
	Points(int no)
	{
		xcoords = new double[no];
		ycoords = new double[no];
		size = no;
		colour=Color.black;
		legend="";
	}
	
	public void set(int i, double x, double y)
	{
		if(i<0 || i> size) return;
		xcoords[i]=x;
		ycoords[i]=y;
	}
	
	public double getX(int i)
	{
		return xcoords[i];
	}
	
	public double getY(int i)
	{
		return ycoords[i];
	}
	
	public int getSize()
	{
		return size;
	}
	
	public void setColour(Color c)
	{
		colour=c;
	}
	
	public Color getColour()
	{
		return colour;
	}
	
	public void setLegend(String l)
	{
		legend=l;
	}
	
	public String getLegend()
	{
		return legend;
	}
}
