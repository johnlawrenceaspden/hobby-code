/*
 * JohnCodebook.java
 */

package com.aspden.tespar.codebooks;
import com.aspden.tespar.*;
import com.aspden.tespar.basics.*;



/**A codebook which takes no account of shape, only of symbol duration*/
public class JohnCodebook implements Codebook {
  
  static int[] squeezearray={0,1,2,3,4,5,6,6,7,7,7,8,8,8,9,9,9,10,10,10,10,11,11,11,11,12,12,12,12,13,13,13,13,13,13,13,14,14,14,14,14,14,};
  static int maxduration=squeezearray.length-1;

  /** Creates new JohnCodebook */
  public JohnCodebook() {
  }
  
  public String getName(){return "John's Codebook";}

  public int getSymbol(int duration, int shape, double magnitude)
  {
    
    if(duration>=maxduration)
    {
      return squeezearray[maxduration]+1;
    }
    else return squeezearray[duration];
  }
  
  public int getCodebookSize()
  {
    return squeezearray[maxduration]+2;
  }

}