/*
 * ScoreForSet.java
 */

package com.aspden.tespar.utility;

import java.text.*;

/**
ScoreForSet implements a cumulative scoring method, when a word is compared with a list of archetypes.
Create a new ScoreForSet, and then register each try. register(0) represents a success. register(1) represents the desired word coming second, etc.
The figure of merit gives exponentially decreasing weight to close results

e.g.

1 is recognised as closest to in order 1,2,3 //success
2 is recognised as closest to 2,3,1          //success
3 is recognised as closet to  2,1,3          //Bad failure. three is seen as 2, with 1 as a second guess.

code:

s=new ScoreForSet()
register(0);
register(0);
register(2);

score = 2/3 = 66% figure of merit= (2+0.25)/3 = 75%
*/

public class ScoreForSet{
  double noOfTries;
  double noOfSuccesses;
  double figureOfMerit;
  
  public ScoreForSet()
  {
    this.noOfTries=0;
    this.noOfSuccesses=0;
    this.figureOfMerit=0;
  }

  /**Allows one to combine two sets of scores to produce combined figures*/
  public void add(ScoreForSet s)
  {
    this.noOfTries+=s.noOfTries;
    this.noOfSuccesses+=s.noOfSuccesses;
    this.figureOfMerit+=s.figureOfMerit;
  }

  /**Let us say that a comparison has ranked the correct word 5th in the list.
  Then call register(5) (list is zero-based) to add 1 to the possible high score,
  nothing to the score (because it didn't come top) and 1/2^5 to the figure of merit
  (because it had a position on the list and every position counts a bit for this)*/
  public void register(int index)
  {
    noOfTries++;
    if(index==0) noOfSuccesses++;
    figureOfMerit+=1.0/(Math.pow(2,index));
  }


  public String toString(){
    NumberFormat f=NumberFormat.getPercentInstance();//new DecimalFormat("##.#");
    f.setMinimumFractionDigits(2);
    return "score: "+f.format(noOfSuccesses/noOfTries)+" figure of merit: "+f.format(figureOfMerit/noOfTries);
  }

}