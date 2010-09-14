import java.awt.event.*;
import java.awt.*;

/**
 * Frame which responds to close messages by shutting down
 * the entire application
 */
public class CloseableFrame extends Frame implements WindowListener
{
	protected CloseableFrame(String s)
	{
		super(s);
		addWindowListener(this);
	}
	
	public void windowOpened(WindowEvent e)	{}
	public void windowClosing(WindowEvent e){System.exit(0);}
	public void windowClosed(WindowEvent e){}
	public void windowIconified(WindowEvent e){}
	public void windowDeiconified(WindowEvent e){}
	public void windowActivated(WindowEvent e){}
	public void windowDeactivated(WindowEvent e){}	
}
