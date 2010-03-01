import scala.swing._
import java.awt._
import javax.swing._

object ScalaFractalTree {
       def main(args: Array[String]){
                      val frame=new JFrame("Scala Fractal Tree")
                      val panel=new MyPanel()
                      frame add panel
                      frame setSize (640, 400)
                      frame setVisible true
                }

       }

class MyPanel extends JPanel{
  override def paintComponent(g:Graphics):Unit = {
    super.paintComponent(g)
    render(g, getWidth(), this.getHeight())
   
  }
  def render(g:Graphics, w:Int, h:Int){
      g.setColor (Color.BLACK)
      g.fillRect( 0, 0, w, h)
      g.setColor (Color.GREEN)
      val initlength=if (w<h) w/5 else h/5
      val branchangle=10*w/h
      val maxdepth=12
      drawtree( g, 0.0, w/2.0, h ,initlength, branchangle, maxdepth)
      }
      
  def drawtree(g:Graphics, angle:Double, x:Double, y:Double, length:Double, 
                             branchangle:Double, depth:Double){
     if (depth>0){
        val newx= x-length*(Math.sin(Math.toRadians( angle)))
        val newy= y-length*(Math.cos(Math.toRadians( angle)))
        val newlength1 = length*(0.75+0.1*Math.random)
        val newlength2 = length*(0.75+0.1*Math.random)
        val newangle1  = angle+branchangle*(0.75+Math.random)
        val newangle2  = angle-branchangle*(0.75+Math.random)
        g.drawLine(x.toInt, y.toInt, newx.toInt, newy.toInt)
        drawtree(g, newangle1, newx, newy, newlength1, branchangle, depth-1)
        drawtree(g, newangle2, newx, newy, newlength2, branchangle, depth-1)
        }
  }
}
                             


