/**
 * For converting between one calendar and another. For instance, to go from
 * a calendar with weekdays only to one with weekends included 
 * mon	tue	......	fri		sat	
 * 0->0 1->1  ....	5->5	6->8
 */
public interface TSExpander
{
	public int getNewIndex(int i);
}
