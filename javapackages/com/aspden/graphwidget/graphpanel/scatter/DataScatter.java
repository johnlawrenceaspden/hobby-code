package com.aspden.graphwidget.graphpanel.scatter;

import com.aspden.graphwidget.linestyles.*;
import com.aspden.graphwidget.rangecalculators.*;
import com.aspden.graphwidget.unitgraph.*;
import com.aspden.graphwidget.unitgraph.objects.*;



import java.util.*;

/**
 * An (ordered) vector of points in R^2
 * which can include its values in Range calculation objects
 * and express itself as a UnitGraphPoints object.
 * It includes a LineStyle object which tells it how to draw itself.
 * y-values may take the special value NaN, used to represent missing data
 * when expressed as a UnitGraphPoints object, corresponding x and y become NaN.
 */
public class DataScatter
{
	private double[] x,y;
	private LineStyle theLineStyle;
	
	/** Create a set of scatter graph points
         * @param x x coordinates
         * @param y y cooordinates
         * @param l style of line to be used to join this set of points.
         */
        public DataScatter(double[] x,double[] y,LineStyle l) {
          if(x.length!=y.length) throw new IllegalArgumentException("Scatter Data Arrays must be same length for DataScatter constructor");
          this.x=(double[]) x.clone();
          this.y=(double[]) y.clone();
          this.theLineStyle=l;
        }
	
	
	/** Add the x-values to a range calculator*/
	public void includeXValuesIn(DoubleRangeCalculator r)
	{
		for(int i=0; i<x.length; i++)
		{
			r.include(x[i]);
		}
	}
	
	/** Add only y-values corresponding to a certain x-range to a range calculator*/
	public void includeYValuesIn(DoubleRangeCalculator r, double xmin, double xmax)
	{
		for(int i=0; i<y.length; i++)
		{
			if(x[i]>xmin && x[i]<xmax)	r.include(y[i]);
		}
	}
	
	/** Given a scale for the whole graph, express the scatter data as
         * a UnitGraphPoints object.
         * I.e. rescale everything to [0,1]x[0,1].
         */
	public UnitGraphPoints getUnitGraphPoints(double xmin, double xmax, double ymin, double ymax)
	{
		double xc[]=new double[y.length];
		double yc[]=new double[y.length];
		for(int i=0; i<y.length; i++)
		{
			if(Double.isNaN(y[i]))
			{
				xc[i]=yc[i]=Double.NaN;
			}
			else
			{
				xc[i]=(x[i]-xmin)/(xmax-xmin);
				yc[i]=(y[i]-ymin)/(ymax-ymin);
			}
		}
		return new UnitGraphPoints(xc,yc,theLineStyle);
	}
}
