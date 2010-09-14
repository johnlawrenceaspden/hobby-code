/**
 * Interface expressing the ability to provide 
 * a String for each element of an array
 */
public interface ArrayLabelGenerator
{
	String getLabel(int i);
	String getMaximalLabel();
}
