public class ScatterData
{
	double x[];
	double y[];
	String name;
	LineStyle style;
	boolean left;
	
	public ScatterData(double x[], double y[], LineStyle s, String name, boolean left)
	{
		this.x=x;
		this.y=y;
		this.style=s;
		this.name=name;
		this.left=left;
	}
	
}
