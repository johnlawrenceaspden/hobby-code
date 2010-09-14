import java.awt.*;
import java.awt.event.*;
import java.text.*;
import java.util.Vector;

/**
 * Main class/Test harness for ScatterGraph Component
 */
public class ScatterApp implements WindowListener, ActionListener, RangeSelectionListener, ItemListener
{
	public static void main(String[] args)
	{
		new ScatterApp();
	}
	
	/**To allow unzooming operations	 */
	Vector zoomHistory = new Vector();
	
	private static final int POINTS_DENSITY=10000;
	
	Frame theFrame;
	Panel controlPanel, graphPanel;
	ScatterGraphComponent theScatterGraph;
	Checkbox lGridBox;
	Checkbox rGridBox;
	Checkbox scaleBox;
	Button undoZoomButton;
	Checkbox lastPointsBox;
	
	Checkbox g1ShowBox,g2ShowBox,g3ShowBox,g4ShowBox;

	Points sinewave=generateSineWave();
	Points randomwalk=generateRandomWalk();
	Points randomwalk2=generateRandomWalk();
	Points tanhwave=generateTanhWave();
	
	public ScatterApp()
	{
		
		sinewave.setColour(Color.blue);
		sinewave.setLegend("Sine");
		tanhwave.setColour(Color.green);
		tanhwave.setLegend("Tanh");
		randomwalk.setColour(Color.orange);
		randomwalk.setLegend("Random Walk");
		randomwalk2.setLegend("Another Random Walk");
		
		//layout
		theFrame=new Frame("ScatterApp");
		theFrame.addWindowListener(this);
		theFrame.setLayout(new BorderLayout());
		
		lGridBox=new Checkbox("Left Grid");
		lGridBox.addItemListener(this);
		
		rGridBox=new Checkbox("Right Grid");
		rGridBox.addItemListener(this);
		
		scaleBox=new Checkbox("All Data");
		scaleBox.addItemListener(this);
		
		undoZoomButton=new Button("UnZoom");
		undoZoomButton.addActionListener(this);
		
		lastPointsBox=new Checkbox("Last Points");
		lastPointsBox.addItemListener(this);
		
		g1ShowBox=new Checkbox("walk",true);
		g1ShowBox.addItemListener(this);
		g2ShowBox=new Checkbox("walk",true);
		g2ShowBox.addItemListener(this);
		g3ShowBox=new Checkbox("tanh",true);
		g3ShowBox.addItemListener(this);
		g4ShowBox=new Checkbox("sin",true);
		g4ShowBox.addItemListener(this);
		
		
		controlPanel=new Panel();
		controlPanel.setBackground(Color.gray);
		controlPanel.add(undoZoomButton);
		controlPanel.add(lGridBox);
		controlPanel.add(rGridBox);
		controlPanel.add(scaleBox);
		controlPanel.add(lastPointsBox);
		controlPanel.add(g1ShowBox);
		controlPanel.add(g2ShowBox);
		controlPanel.add(g3ShowBox);
		controlPanel.add(g4ShowBox);

		theFrame.add("South",controlPanel);
		
		graphPanel=new Panel();
		graphPanel.setLayout(new GridLayout(4,1));
		
		graphPanel.setBackground(Color.gray);
		graphPanel.add(g1ShowBox);
		graphPanel.add(g2ShowBox);
		graphPanel.add(g3ShowBox);
		graphPanel.add(g4ShowBox);

		theFrame.add("East",graphPanel);
		
		
		
		
		
		theScatterGraph = new ScatterGraphComponent();
		theScatterGraph.addRangeSelectionListener(this);
		theFrame.add("Center", theScatterGraph);
		
		theFrame.pack();
		theFrame.show();
		
		theScatterGraph.addLabel("Random Walks and", Compass.NW);
		theScatterGraph.addLabel("Mathematical Functions", Compass.NW);
		theScatterGraph.setLabelColour(Color.orange, Compass.NW);
		theScatterGraph.setLabelFont(new Font("Roman", Font.ITALIC, 12), Compass.NW);
		
		theScatterGraph.setLabelFont(new Font("Courier", Font.BOLD, 10), Compass.SE);
		theScatterGraph.setLabelColour(Color.red, Compass.SE);
		
		SelectGraphs();
		
		RangeSelected(-10.0,10.0);
		
	}

	private void SelectGraphs()
	{
		theScatterGraph.removeAllData();
		if (g1ShowBox.getState()) theScatterGraph.addRData(randomwalk);
		if (g2ShowBox.getState()) theScatterGraph.addRData(randomwalk2);
		if (g3ShowBox.getState()) theScatterGraph.addLData(tanhwave);
		if (g4ShowBox.getState()) theScatterGraph.addLData(sinewave);
		
		if(g1ShowBox.getState()||g2ShowBox.getState())
		{
			rGridBox.setEnabled(true);
		}
		else
		{
			rGridBox.setEnabled(false);
			rGridBox.setState(false);
			theScatterGraph.setRGridIsShowing(false);
		}
		if(g3ShowBox.getState()||g3ShowBox.getState())
		{
			lGridBox.setEnabled(true);
		}
		else
		{
			lGridBox.setEnabled(false);
			lGridBox.setState(false);
			theScatterGraph.setLGridIsShowing(false);
		}
		

	}

	public void windowOpened(WindowEvent e)	{}
	public void windowClosing(WindowEvent e){System.exit(0);}
	public void windowClosed(WindowEvent e){}
	public void windowIconified(WindowEvent e){}
	public void windowDeiconified(WindowEvent e){}
	public void windowActivated(WindowEvent e){}
	public void windowDeactivated(WindowEvent e){}
	

	
	
	/**
	 * Handles the actions from the buttons
	 */
	public void actionPerformed(ActionEvent e)
	{
		if(e.getSource()==undoZoomButton)
		{
			unZoom();
		}
	}
	
	/**
	 * Should have a record for current state and previous state
	 * Restore previous and delete current
	 * Otherwise operation is not valid so don't do anything.
	 */
	private void unZoom()
	{
		int i=zoomHistory.size();
		
		if (i>=2)
		{
			zoomHistory.removeElementAt(i-1);
			Range g = (Range) zoomHistory.elementAt(i-2);
			try{ RescaleGraph(g.a, g.b);}
			catch(RangeTooSmallException e){}
		}
		else return;
	}
	
	/**
	 * Reset the Graph Object, and then display the new range.
	 */
	void RescaleGraph(double x1,double x2) throws RangeTooSmallException
	{
		theScatterGraph.setRange(x1,x2);
		theScatterGraph.setChooseOwnScale(false);
		
		NumberFormat f = NumberFormat.getNumberInstance();
		String l= "("+f.format(x1) +","+f.format(x2)+")";
		
		theScatterGraph.removeLabels(Compass.SE);
		theScatterGraph.addLabel(l, Compass.SE);
		theScatterGraph.showData();
		resetControls();
	}

	/**
	 * On range event, rescale, 
	 * keep a history for unzooming &
	 * make ranges the right way up!
	 */
	public void RangeSelected(double x1,double x2)
	{
		double t;
		
		if (x1>x2){t=x1; x1=x2; x2=t;}
		
		try{
			RescaleGraph(x1,x2);
			zoomHistory.addElement(new Range(x1,x2));
		} catch (RangeTooSmallException e){}
			
		
	}
	
	private Points generateSineWave()
	{
		final int samples = POINTS_DENSITY;
		
		double x = -2.0*Math.PI;
		double dx = (4.0*Math.PI)/samples;
		
		Points p= new Points(samples);
		for (int i = 0; i < samples; i++)
		{
			p.set(i, x, Math.sin(x));
			x+=dx;
		}
		return p;
	}
	
	private Points generateTanhWave()
	{
		final int samples = POINTS_DENSITY;
		
		double x = -2.0*Math.PI;
		double dx = (4.0*Math.PI)/samples;
		
		Points p= new Points(samples);
		for (int i = 0; i < samples; i++)
		{
			p.set(i, x, (Math.exp(x)-Math.exp(-x))/(Math.exp(x)+Math.exp(-x)));
			x+=dx;
		}
		return p;
	}

	private Points generateRandomWalk()
	{
		final int samples = POINTS_DENSITY;
		
		double x = -10;
		double dx = 20.0/samples;
		double y =(Math.random()-0.5);
		
		Points p= new Points(samples);
		for (int i = 0; i < samples; i++)
		{
			p.set(i, x,y);
			x+=dx*Math.random()*2.0;
			y+=(Math.random()-0.5)/Math.sqrt(samples);
		}
		return p;
	}

	private void resetControls()
	{
		lGridBox.setState(theScatterGraph.getLGridIsShowing());
		rGridBox.setState(theScatterGraph.getRGridIsShowing());
		scaleBox.setState(theScatterGraph.getChooseOwnScale());
		lastPointsBox.setState(theScatterGraph.getLastPointsAreShowing());
	}
	
	private void controlsChanged()
	{
		theScatterGraph.setLGridIsShowing(lGridBox.getState());
		theScatterGraph.setRGridIsShowing(rGridBox.getState());
		theScatterGraph.setChooseOwnScale(scaleBox.getState());
		theScatterGraph.setLastPointsAreShowing(lastPointsBox.getState());
		SelectGraphs();
	}
	
	public void itemStateChanged(ItemEvent e)
	{
		controlsChanged();
		theScatterGraph.showData();
	}
}
