import java.text.*;

/**
 * This is class which is really a module. 
 * Explicitly to contain functions that don't have associated data.
 * The term spectral class is from the 'things to be avoided' list in 
 * OO Design in Java.
 */
public class SpectralClass
{
	/**
	 * Given a range, and a number, return an appropriate decimal representation.
	 * For instance 0.123456789 is 0.13 in a range of length 1, but 0.12345 in one of length 0.01
	 * however won't do more than 12 sf as that would be silly.
	 */
	public static String getCoordText(double range, double x)
	{
		return getLabel(range, x);
	}
	
	/**Here we're assuming that all labels are the same length, but playing safe in case some 
	 * JVM decides to cut off trailing zeroes.
	 */
	public static String getMaximalCoordText(double range)
	{
		return getLabel(range, 1.111111111111111111111);
	}
	
	private static String getLabel(double range, double x)
	{
		//How many s.f?
		if(range<0)range=-range;
		double number=100;
		String format="0.";
		
		while(number>range && number>1e-10)
		{
			format+="0";
			number/=10;
		}
		DecimalFormat nf= new DecimalFormat(format);
		return nf.format(x);
	}
}
