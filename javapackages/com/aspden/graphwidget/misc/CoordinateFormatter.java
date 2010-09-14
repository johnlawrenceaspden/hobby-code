package com.aspden.graphwidget.misc;


import java.text.*;

/** Formats coordinates appropriately for their range.
 * E.g. in the range 0-1, 0.1234567 should appear as 0.12, whereas in the range 0.12-0.13 it should be 0.1234.
 * however we draw the line at 12 s.f.
 */
public class CoordinateFormatter
{
    private DecimalFormat theFormat;

    /** Creates a formatter for the specified range
     * @param range The length of the range. E.g. if the range is 1.2-1.3, the length is 0.1
     */
    public CoordinateFormatter(double range)
    {
        //How many s.f?
        if(range<0)range=-range;
        double number=100;
        String format="0";

        if(number>range && number>1e-10) format+=".";
        while(number>range && number>1e-10)
        {
            format+="0";
            number/=10;
        }
        theFormat= new DecimalFormat(format);
    }

    /** Get the appropriately formatted coordinate.
     * @param x The coordinate to be formatted.
     * @return Formatted string.
     */
    public String getCoordText(double x)
    {
        return theFormat.format(x);
    }

    /** Get the longest possible label for the given range.
     * @return The longest possible coordinate string.
     */
    public String getMaximalCoordText()
    {
        return theFormat.format(1.111111111111111111111);
    }

}
