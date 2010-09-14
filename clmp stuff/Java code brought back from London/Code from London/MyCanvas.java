
import java.awt.Canvas;
import java.awt.Color;
import java.awt.Graphics;
import java.util.Random;
import java.awt.Image;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.lang.Double;
import java.awt.event.*;
import java.text.*;


class MyCanvas extends Canvas
{
	private Image offScreenImage;
	
	private static final DecimalFormat onePlace = new DecimalFormat("###.#");

	
	private static final int points=100;
	private static final int noOfSeries=6;
	
	private double a[][][]=new double [noOfSeries][points][2];
	private LineStyle lineStyles[]=new LineStyle[noOfSeries];
	

	private double ymin, ymax;
	private double xmin, xmax;
	


	public MyCanvas()
	{
		setSize(200, 200);
		setBackground(Color.white);
		Random r = new Random();
		
		for (int series=0; series<noOfSeries; series++)
		{
			lineStyles[series]=new LineStyle();
		}
		lineStyles[0]=new LineStyle(LineStyle.BLUEDASHED);
		lineStyles[1]=new LineStyle(LineStyle.BLUELINE);
		lineStyles[2]=new LineStyle(LineStyle.THICKBLACKDASHED);
		lineStyles[3]=new LineStyle(LineStyle.BLUEYELLOWSTRIPE);
									
		
		
		 
		/*initial values*/
		for (int series=0;series<noOfSeries;series++)
		{
			a[series][0][0]=-points/2;
			a[series][0][1]=0;
		}
		ymax=ymin=a[0][0][1];
		xmax=xmin=a[0][0][0];
		
		/*random walk*/
		for (int series=0;series<noOfSeries;series++)
		{
			for(int i=1;i<points;i++)
			{
				a[series][i][0]=a[series][i-1][0]+1.0;
				a[series][i][1]=a[series][i-1][1]+(r.nextDouble()-0.5);
			}
		}
		
		/*Calculate graph ranges*/
		ymax=ymin=a[0][0][1];
		xmax=xmin=a[0][0][0];
		for (int series=0;series<noOfSeries;series++)
		{
			for(int i=1;i<points;i++)
			{
				a[series][i][0]=a[series][i-1][0]+1.0;
				a[series][i][1]=a[series][i-1][1]+(r.nextDouble()-0.5);
			
				if(ymin>a[series][i][1])ymin=a[series][i][1];
				if(ymax<a[series][i][1])ymax=a[series][i][1];
				if(xmin>a[series][i][0])xmin=a[series][i][0];
				if(xmax<a[series][i][0])xmax=a[series][i][0];
			}
		}
	}
	
	private int xcoord(double x)
	{
		int width=getSize().width;
		double xscale=width / (xmax-xmin);
		return (int)((x-xmin)*xscale);
	}
	
	private int ycoord(double y)
	{
		int height=getSize().height;
		double yscale=height / (ymax-ymin);
		return height-(int)((y-ymin)*yscale);
	}

	private double gettickspacing(double width, int desired)
	{
		double guess=width/desired;
		double spacing=1.0;
		while (spacing>guess) spacing/=10;
		while (spacing<guess) spacing *=10;
		
		while (spacing/2>guess) spacing/=2;
		return spacing;
	}
	
	public void paint(Graphics g)
	{
		super.paint(g);
		
		/*Create an off screen buffer to draw in*/
		offScreenImage=createImage(getSize().width,getSize().height);
		Graphics gr = offScreenImage.getGraphics();
		
	
		/*Axes*/
		gr.setColor(Color.black);
		int xaxisy=ycoord(0);
		gr.drawLine(xcoord(xmin),xaxisy,xcoord(xmax),xaxisy);
		int yaxisx=xcoord(0);
		gr.drawLine(yaxisx,ycoord(ymin),yaxisx,ycoord(ymax));

		/*X axis ticks and labels*/
		double tickspace=gettickspacing(xmax-xmin,getSize().width/40);
		
		gr.setColor(Color.red);
		gr.drawString("0",xcoord(0),xaxisy); //doesn't get done below???!!!
		
		for(double x=0;(x<xmax || -x>-xmin);x+=tickspace)
		{
			gr.setColor(Color.red);
			gr.drawString(onePlace.format(x),xcoord(x),xaxisy);
			gr.drawString(onePlace.format(-x),xcoord(-x),xaxisy);
			gr.setColor(Color.black);
			gr.drawLine(xcoord(x),xaxisy,xcoord(x),xaxisy+(getSize().height/200)+1);
			gr.drawLine(xcoord(-x),xaxisy,xcoord(-x),xaxisy+(getSize().height/200)+1);
		}
		
		/*Y axis labels*/
		for(double y=xmin;y<ymax;y+=(ymax-ymin)/(getSize().height/40))
		{
			gr.drawString(onePlace.format(y),yaxisx,ycoord(y));
		}

		/*Plots*/
		for (int series=0; series<noOfSeries; series++)
		{
			for(int i=1; i<points; i++)
			{
				int x1, y1, x2, y2;

				x1=xcoord(a[series][i-1][0]); x2=xcoord(a[series][i][0]);
				y1=ycoord(a[series][i-1][1]); y2=ycoord(a[series][i][1]);
				myLineDraw(gr,x1,y1,x2,y2,lineStyles[series]);
			}
		}
		g.drawImage(offScreenImage, 0, 0, this);
	}
	
	/*this needs to be rewritten using a plot function rather than 
	drawing one-point lines*/
	private void myLineDraw(Graphics gr, int x1, int y1, int x2, int y2, LineStyle l)
	{
		if(l.striping==0)
		{
			gr.setColor(l.colour1);
			for(int i=0; i<= l.thickness; i++) gr.drawLine(x1,y1-i,x2,y2-i);
			
			return;
		}
		
		if(x1==x2)
		{
			if((x1/l.striping)/2*2==(x1/l.striping)){gr.setColor(l.colour1);} else {gr.setColor(l.colour2);} 
			if(y1>y2)
			{
				gr.drawLine(x1,y1,x2,y2-l.thickness);	
			}
			else
			{
				gr.drawLine(x1,y1-l.thickness,x2,y2);	
			}
				
			return;
		}
		
		if (x1>x2)//always draw left to right
		{
			int t;
			t=x1; x1=x2; x2=t;
			t=y1; y1=y2; y2=t;
		}
		
		int xlen=x2-x1;
		int ylen=y2-y1;
		int xrat,yrat;
		int x,y;
		
		x=x1;y=y1;
		
		int xrect=0, yrect=0;
			
		if(ylen>=0)
		{
			while(x<=x2)
			{	
				if((x/l.striping)/2*2==(x/l.striping)){gr.setColor(l.colour1);} else {gr.setColor(l.colour2);}
				gr.drawLine(x,y,x,y-l.thickness);
				if(xrect>yrect)
				{
					y+=1;
					yrect+=xlen;
				}
				else if (xrect==yrect)
				{
					x+=1;y+=1;
					yrect+=xlen;
					xrect+=ylen;
				}
				else 
				{
					x+=1;
					xrect+=ylen;
				}
			}
		}
		else
		{
			while(x<=x2)
			{
				if((x/l.striping)/2*2==(x/l.striping)){gr.setColor(l.colour1);} else {gr.setColor(l.colour2);}
				gr.drawLine(x,y,x,y-l.thickness);
				
				xrat=ylen*(x-x1);
				yrat=xlen*(y-y1);
				if(xrect<yrect)
				{
					y-=1;
					yrect-=xlen;
				}
				else if (xrect==yrect)
				{
					x+=1;y-=1;
					yrect-=xlen;
					xrect+=ylen;
					
				}
				else 
				{
					x+=1;
					xrect+=ylen;
				}
			}
		}
	}
}


