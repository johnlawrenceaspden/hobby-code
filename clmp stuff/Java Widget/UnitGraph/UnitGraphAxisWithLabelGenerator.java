/**
 * A base class for axes to which can take their labels from an external source.
 */
abstract public class UnitGraphAxisWithLabelGenerator extends UnitGraphAxis
{
	private UnitGraphAxisLabelGenerator theAxisLabelGenerator;
	
	public UnitGraphAxisWithLabelGenerator(int orientation, boolean gridlines, UnitGraphAxisLabelGenerator a, boolean leftlabels)
	{
		super(orientation, gridlines, leftlabels);
		theAxisLabelGenerator=a;
	}
	
	protected String getLabel(double x)
	{
		if(theAxisLabelGenerator!=null)
		{
			return theAxisLabelGenerator.getLabel(x);
		}
		else return null;
	}
	
	protected String getMaximalLabel()
	{
		if(theAxisLabelGenerator!=null)
		{
			return theAxisLabelGenerator.getMaximalLabel();
		}
		else return null;
	}
}
