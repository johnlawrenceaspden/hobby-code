/*
 * Codebook.java
 */
package com.aspden.tespar.codebooks;

/** Specifies the responsibilities of a TESPAR codebook, which is a way of getting from (shape, duration, magnitude) , a <I>natural</I> TESPAR symbol, to a single code representing the symbol.
 */
public interface Codebook {

    /** Get the symbol (according to this Codebook) for a particular natural symbol.
     * @param duration duration of the natural symbol.
     * @param shape shape of the natural symbol.
     * @param magnitude magnitude of the natural symbol.
     * @return the Codebook symbol.
     */
    public abstract int getSymbol(int duration, int shape, double magnitude);
    /** get the size of the Codebook, <I>ie</I> the number of different Codebook symbols.
     * @return the size of this Codebook.
     */
    public abstract int getCodebookSize();
    /** Get the name of the Codebook.
     * @return the name of the Codebook.
     */
    public abstract String getName();
}

