package com.aspden.graphwidget.demo;


import com.aspden.graphwidget.misc.*;
import com.aspden.graphwidget.unitgraph.*;
import com.aspden.graphwidget.linestyles.*;
import com.aspden.graphwidget.unitgraph.objects.*;
import com.aspden.graphwidget.unitgraph.objects.axes.*;

import java.awt.*;
import java.text.*;

public class UnitSquareApp
{
    private CloseableFrame theFrame=new CloseableFrame("Unit Square");
    private UnitGraphWithMouseSelect theGraph=new UnitGraphWithMouseSelect();
    private Label theLabel = new Label();

    public static void main(String[] args)
    {
        new UnitSquareApp();
    }

    public UnitSquareApp()
    {
        theGraph.setMouseLabelGenerator(new UnitGraphMouseLabelGenerator()
        {
            public String getMouseLabel(double x, double y)
            {
                DecimalFormat sf2=new DecimalFormat("0.00");
                return sf2.format(x)+" "+sf2.format(y);
            }
        });
        theGraph.addRangeSelectionListener( new UnitGraphRangeSelectionListener()
        {
            public void rangeSelected(double x1, double y1, double x2, double y2)
            {
                theLabel.setText(" "+x1+" "+y1+" "+x2+" "+y2);
            }
            public void rightButtonReleased(double x, double y){
            }
        });

        theGraph.setFont(new Font("Courier", Font.BOLD,40));

        theGraph.addElement(new UnitGraphLabel(new Compass(Compass.C), "Central Label"));
        theGraph.addElement(new UnitGraphLabel(new Compass(Compass.NW),"Northwesterly Label" ));
        {
            UnitGraphLabel l=new UnitGraphLabel(new Compass(Compass.E));
            l.addString("Easterly");
            l.addString("Label");
            l.addString("On three lines");
            theGraph.addElement(l);
        }

        {
            UnitGraphLegendBox a=new UnitGraphLegendBox(new Compass(Compass.SW));
            a.addLegend(new BobblyLineStyle(Color.magenta, Color.orange),"Fictional");
            a.addLegend(new BobblyLineStyle(Color.pink, Color.blue),"Second Fictional");
            theGraph.addElement(a);
        }

        UnitGraphConstantAxis a=new UnitGraphConstantAxis(new Compass(Compass.N), false);
        a.addTick(0.5, 1, "MyTick");
        theGraph.addElement(a);

        theGraph.addElement(new UnitGraphFloatingPointAxis(new Compass(Compass.E), false, 0,100));
        theGraph.addElement(new UnitGraphEvenLabelAxis(new Compass(Compass.S), true, new SillyLabeller()));
        theGraph.addElement(new UnitGraphFloatingPointAxis(new Compass(Compass.W), true, 5,0));

        theGraph.addElement(UnitGraphPoints.getSample(300));

        theFrame.setLayout(new BorderLayout());
        theFrame.add(theGraph, "Center");
        theFrame.add(theLabel, "South");
        theFrame.setSize(200,200);
        theFrame.show();
    }

    private class SillyLabeller implements UnitGraphAxisLabelGenerator
    {
        public String getLabel(double x)
        {
            return "silly";
        }
        public String getMaximalLabel()
        {
            return "very silly";
        }
    }
}
