import java.awt.Frame;
import java.awt.Button;
import java.awt.GridLayout;

class MyGraph
{
	public static void main(String argv[])
	{
		Frame f = new Frame("mygraph");
		MyCanvas c = new MyCanvas();
		GridLayout gl = new GridLayout(1, 1);
		f.setLayout(gl);
		//f.add(new Button("hello"));
		f.add(c);
		f.pack();
		f.show();
		c.initBuffer();
	}
}


