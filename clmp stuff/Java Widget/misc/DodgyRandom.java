/**
 * A random class that returns integers in a range.
 * Slight sacrifice of distribution explains name.
 */
public class DodgyRandom extends java.util.Random
{
	/**
	 * nextInt(7) returns one of 0,1,2,3,4,5,6.
	 * Note the distribution is disturbed.
	 */
	public int nextInt(int range)
	{
		int a = this.nextInt();
		if(a<0) a=-a;
		a = a % range;
		return a;
	}
}
