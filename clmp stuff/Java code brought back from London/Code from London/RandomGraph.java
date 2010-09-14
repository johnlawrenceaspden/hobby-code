import java.awt.Frame;
import java.awt.Button;
import java.awt.GridLayout;
import java.awt.event.WindowListener;


class RandomGraph implements WindowListener
{
	private Frame f;
	private MyCanvas c;
	private GridLayout gl;
	
	
	public static void main(String argv[])
	{
		new RandomGraph();
	}
	
	public RandomGraph()
	{
		f = new Frame("mygraph");
		c = new MyCanvas();
		gl = new GridLayout(1, 1);
		
		f.setLayout(gl);
		f.add(new Button("hello"));
		f.addWindowListener(this);
		f.add(c);
		//f.pack();
		f.setVisible(true);
	}
	
	

	public void windowDeactivated(java.awt.event.WindowEvent p1)
	{
		// TODO: Add your own implementation.
	}

	public void windowClosing(java.awt.event.WindowEvent p1)
	{
		f.setVisible(false);
		f.dispose();
		System.exit(0);
	}

	public void windowOpened(java.awt.event.WindowEvent p1)
	{
		// TODO: Add your own implementation.
	}

	public void windowClosed(java.awt.event.WindowEvent p1)
	{
		// TODO: Add your own implementation.
	}

	public void windowDeiconified(java.awt.event.WindowEvent p1)
	{
		// TODO: Add your own implementation.
	}

	public void windowActivated(java.awt.event.WindowEvent p1)
	{
		// TODO: Add your own implementation.
	}

	public void windowIconified(java.awt.event.WindowEvent p1)
	{
		// TODO: Add your own implementation.
	}
}

