package nmsd3

import java.awt.Color._
import java.awt.{Dimension, Toolkit}
import javax.swing.JFrame

import nmsd3.Common._
import org.math.plot._


object Graph {
  var plot: Plot3DPanel = new Plot3DPanel()
  val sSize: Dimension = Toolkit.getDefaultToolkit().getScreenSize();
  val frame: JFrame = new JFrame("Plot panel")
  frame.setSize(sSize)
  plot.setSize(sSize)
  frame.setVisible(true)
  frame.setContentPane(plot)
  frame.setFocusableWindowState(true)

  val delay = 400

  val resize = 1

  def paint(graph: (Map[Int, List[(Int, Int, Double)]], Map[Int, List[(Int, Int, Double)]]),
            graphW: (Map[Int, List[(Int, Int, Double)]], Map[Int, List[(Int, Int, Double)]])) = {
    (0 to nT - 1).foreach(t => {
//      plot.addScatterPlot("PlotV1", red,
//        graph._1(t).map(point => deltaX * point._1).toArray,
//        graph._1(t).map(point => deltaY * point._2).toArray,
//        graph._1(t).map(point => point._3 / resize).toArray)
      plot.addScatterPlot("PlotV2", red,
        graph._2(t).map(point => deltaX * point._1).toArray,
        graph._2(t).map(point => deltaY * point._2).toArray,
        graph._2(t).map(point => point._3 / resize).toArray)
//      plot.addScatterPlot("PlotW1", blue,
//        graphW._1(t).map(point => deltaX * point._1).toArray,
//        graphW._1(t).map(point => deltaY * point._2).toArray,
//        graphW._1(t).map(point => point._3 / resize).toArray)
      plot.addScatterPlot("PlotW2", blue,
        graphW._2(t).map(point => deltaX * point._1).toArray,
        graphW._2(t).map(point => deltaY * point._2).toArray,
        graphW._2(t).map(point => point._3 / resize).toArray)
      if (t != nT) removeWithDelay
    })
  }

  def removeWithDelay = {
    val start = System.currentTimeMillis
    while (start + delay > System.currentTimeMillis) {}
    plot.removeAllPlots()
  }

}
