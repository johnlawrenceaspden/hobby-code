import java.awt.Canvas;
import java.awt.Color;
import java.awt.Graphics;
import java.util.Random;
import java.awt.Image;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.event.ComponentListener;
import java.awt.event.ComponentEvent;

class MyCanvas extends Canvas implements ComponentListener
{
	private Image offScreenImage;
	private Graphics offScreenGraphics;
	private boolean buf=false;

	public MyCanvas()
	{
		addComponentListener(this);
		setSize(600, 600);
		setBackground(Color.white);
	}

	public void initBuffer()
	{
		offScreenImage = createImage(getSize().width, getSize().height);
		offScreenGraphics = offScreenImage.getGraphics();
	}

	public void paint(Graphics g)
	{
		System.out.println("painting...");
		super.paint(g);

		Graphics gr;

		if(buf)
			gr = offScreenGraphics;
		else
			gr = g;

		gr.setColor(Color.black);
		Random r = new Random();
		int x1, y1, x2, y2, i;
		for(i=0; i<20000; i++)
		{
			x1=r.nextInt()%590;
			y1=r.nextInt()%590;
			x2=r.nextInt()%590;
			y2=r.nextInt()%590;
			gr.drawLine(x1, y1, x2, y2);
		}

		if(buf)
			g.drawImage(offScreenImage, 0, 0, this);
		buf = !buf;
		System.out.println("done");
	}

	public void update(Graphics g)
	{
		System.out.println("updating...");
		super.update(g);
		System.out.println("done");
	}

	public void setSize(int w, int h)
	{
		System.out.println("size " + w + " " + h);
		super.setSize(w, h);
	}

	public void setSize(Dimension d)
	{
		System.out.println("size2");
		super.setSize(d);
	}
		
	public void setBounds(int x, int y, int w, int h)
	{
		System.out.println("bounds " + w + " " + h);
		super.setBounds(x, y, w, h);
	}

	public void setBounds(Rectangle r)
	{
		System.out.println("bounds2");
		super.setBounds(r);
	}

	public void componentResized(ComponentEvent e)
	{
		System.out.println("resize event");
	}

	public void componentMoved(ComponentEvent e)
	{
		System.out.println("move event");
	}

	public void componentShown(ComponentEvent e)
	{
		System.out.println("show event");
	}

	public void componentHidden(ComponentEvent e)
	{
		System.out.println("hide event");
	}
}


