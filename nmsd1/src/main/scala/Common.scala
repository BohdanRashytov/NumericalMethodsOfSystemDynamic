package nmsd1

import java.lang.Math._
import nmsd1.Graph._

object Common {
  // function
  val alpha = 1
  val a = 1
  val A = 1
  val k1 = 1
  val k2 = 1

  // x and y
  val N = 10
  val l1 = 1.0
  //x
  val hx = l1 / N
  val l2 = 1.0
  //y
  val hy = l2 / N

  // time
  val M = 100
  val tau = 1.0 / M

  // List[Double]
  val x = (0 to N).toList
  val y = (0 to N).toList
  val t = (0 to M).toList

  def w(kx: Double, my: Double, vt: Double): Double =
    A * exp(k1 * kx * hx + k2 * my * hy + (k1 * k1 + k2 * k2) * a * vt * tau)

  def f(kx: Double, my: Double, vt: Double): Double =
    A * (k1 * k1 + k2 * k2) * (a - alpha) * exp(k1 * kx * hx + k2 * my * hy + (k1 * k1 + k2 * k2) * a * vt * tau)

  def graphW: (Map[Int, List[(Int, Int, Double)]], Map[Int, List[(Int, Int, Double)]]) = {
    var graph1: Map[Int, List[(Int, Int, Double)]] = initialConditions
    var graph2: Map[Int, List[(Int, Int, Double)]] = initialConditions
    t.foreach(t => {
      var list1: List[(Int, Int, Double)] = List()
      var list2: List[(Int, Int, Double)] = List()
      x.foreach(x =>
        y.foreach(y =>
          list1 = list1.::((x, y, w(x, y, t)))
        )
      )
      y.foreach(y =>
        x.foreach(x =>
          list2 = list2.::((x, y, w(x, y, t)))
        )
      )
      graph1 = graph1.+(t -> list1)
      graph2 = graph2.+(t -> list2)
    })
    (graph1, graph2)
  }

  def initialConditions: Map[Int, List[(Int, Int, Double)]] = {
    var graph: Map[Int, List[(Int, Int, Double)]] = Map()
    // t = 0
    var list: List[(Int, Int, Double)] = List()
    x.foreach(x => {
      y.foreach(y =>
        list = list.::((x, y, w(x, y, 0)))
      )
    })
    graph = graph.+(0 -> list)

    //t != 0
    t.foreach(t => if (t != 0) {
      var list: List[(Int, Int, Double)] = List()
      //x = 0 or x = 1
      y.foreach(y => {
        list = list.::((0, y, w(0, y, t)))
        list = list.::((N, y, w(N, y, t)))
      })
      //y = 0 or y = 1 and x != 0 and x != 1
      x.foreach(x => if (x != 0 && x != N) {
        list = list.::((x, 0, w(x, 0, t)))
        list = list.::((x, N, w(x, N, t)))
      })
      graph = graph.+(t -> list)
    })
    graph
  }

  val initialConditionsForV: Map[Int, Map[(Int, Int), Double]] = {
    var v: Map[Int, Map[(Int, Int), Double]] = Map()
    val graph: Map[Int, List[(Int, Int, Double)]] = initialConditions
    graph.keys.foreach(t => {
      var tempMap: Map[(Int, Int), Double] = Map()
      (0 to graph(t).length - 1)
        .foreach(i => tempMap = tempMap.+((graph(t)(i)._1, graph(t)(i)._2) -> graph(t)(i)._3))
      v = v.+(t -> tempMap)
    })
    v
  }

  def functionToGraph(v: Map[Int, Map[(Int, Int), Double]]):
  (Map[Int, List[(Int, Int, Double)]], Map[Int, List[(Int, Int, Double)]]) = {
    var graph1: Map[Int, List[(Int, Int, Double)]] = Map()
    var graph2: Map[Int, List[(Int, Int, Double)]] = Map()
    v.keys.foreach(t => {
      var list1: List[(Int, Int, Double)] = List()
      var list2: List[(Int, Int, Double)] = List()
      y.foreach(y =>
        x.foreach(x =>
          list1 = list1.::((x, y, v(t)(x, y)))
        )
      )
      x.foreach(x =>
        y.foreach(y =>
          list2 = list2.::((x, y, v(t)(x, y)))
        )
      )
      graph1 = graph1.+(t -> list1)
      graph2 = graph2.+(t -> list2)
    })
    (addInitialConditions(graph1), addInitialConditions(graph2))
  }
}
