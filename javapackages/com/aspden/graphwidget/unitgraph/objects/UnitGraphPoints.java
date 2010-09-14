package com.aspden.graphwidget.unitgraph.objects;

import com.aspden.graphwidget.linestyles.*;

import java.util.*;
import java.awt.*;

/**
 * A collection of points in the unit square which know how to draw themselves.
 */
public class UnitGraphPoints implements UnitGraphObject
{
	private LineStyle l;
	private double x[];
	private double y[];
	
	/** Create a Points object from arrays and a LineStyle
         * @param x x coordinates of points in range [0,1]. NaN <-> invalid point.
         * @param y y coordinates of points in range [0,1]. NaN <-> invalid point.
         * @param l {@link LineStyle} of points.
         */
	public UnitGraphPoints(double x[], double y[], LineStyle l)
	{
		if(x.length!=y.length || x.length==0)
		{
			throw new IllegalArgumentException();
		}
		this.x=x;
		this.y=y;
		//this.x=(double[])x.clone();
		//this.y=(double[])y.clone();
		this.l=l ;
	}
	
	public void draw(Graphics g, int w, int h)
	{
		int[] xc=new int[x.length];
		int[] yc=new int[x.length];
		boolean[] valid=new boolean[x.length];
		
		for(int i=0;i<x.length; i++)
		{
		    if(Double.isNaN(x[i]))
		    {
		    	valid[i]=false;
		    }
		    else
		    {
		    	valid[i]=true;
		    	xc[i]=(int)(w*x[i]); 
			yc[i]=h-(int)(h*y[i]);
		    }
		}
		
		l.drawLine(g, xc, yc, valid);		
	}
	
	/** Create an instance of the Points class for testing purposes.
         * @param len How many points in the sample?
         * @return A random UnitGraphPoints of length len.
         */
	public static UnitGraphPoints getSample(int len)
	{
		Random r=new Random();
		
		double x[]=new double[len];
		double y[]=new double[len];
		x[0]=y[0]=0.0;
		for(int i=1;i<len;i++)
		{
			x[i]=x[i-1]+2*r.nextDouble()/len;
			y[i]=y[i-1]+2*r.nextDouble()/len;
		}
		for(int i=0;i<len;i++)
		{
			if(r.nextDouble()<0.1) {x[i]=Double.NaN; y[i]=Double.NaN;}
		}
		
		LineStyle l = new BobblyLineStyle(Color.blue, Color.red);
		return new UnitGraphPoints(x,y,l);
	}
}
