package nmsd1

import javax.swing.JFrame

import org.math.plot._
import java.awt.Color._
import nmsd1.Common._


object Graph {
  var plot: Plot3DPanel = new Plot3DPanel()
  var plotError: Plot2DPanel = new Plot2DPanel()
  plot.setSize(1, 1)

  val frame: JFrame = new JFrame("Plot panel")
  frame.setSize(600, 600)
  frame.setVisible(true)
  frame.setContentPane(plot)

  val frameError: JFrame = new JFrame("Error panel")
  frameError.setSize(600, 600)
  frameError.setVisible(true)
  frameError.setContentPane(plotError)

  val delay = 1000

  val resize = 1

  def paint(graph: (Map[Int, List[(Int, Int, Double)]], Map[Int, List[(Int, Int, Double)]]),
            error: Map[Int, Double]) = {
    val timeArray = t.map(k => k * tau).::(0.0)
    val errorArray = t.map(t => (error(t))).::(0.0)
    (0 to M).foreach(t => {
      plotError.addLinePlot("Error", timeArray.slice(0, t + 2).toArray,
        errorArray.slice(0, t + 2).toArray)
      plot.addLinePlot("PlotV1", red,
        graph._1(t).map(point => hx * point._1).toArray,
        graph._1(t).map(point => hy * point._2).toArray,
        graph._1(t).map(point => point._3 / resize).toArray)
      plot.addLinePlot("PlotV2", red,
        graph._2(t).map(point => hx * point._1).toArray,
        graph._2(t).map(point => hy * point._2).toArray,
        graph._2(t).map(point => point._3 / resize).toArray)
      plot.addLinePlot("PlotW1", blue,
        graphW._1(t).map(point => hx * point._1).toArray,
        graphW._1(t).map(point => hy * point._2).toArray,
        graphW._1(t).map(point => point._3 / resize).toArray)
      plot.addLinePlot("PlotW2", blue,
        graphW._2(t).map(point => hx * point._1).toArray,
        graphW._2(t).map(point => hy * point._2).toArray,
        graphW._2(t).map(point => point._3 / resize).toArray)
      if (t != M) removeWithDelay
    })
  }

  def removeWithDelay = {
    val start = System.currentTimeMillis
    while (start + delay > System.currentTimeMillis) {}
    plot.removeAllPlots()
    plotError.removeAllPlots()
  }

}
