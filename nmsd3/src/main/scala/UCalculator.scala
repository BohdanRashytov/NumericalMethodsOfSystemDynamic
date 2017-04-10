package nmsd3

import nmsd3.Common._

object UCalculator {
  private def f(x: Int, y: Int, t: Int) =
    -(p(t)(x, y) - p(t)(x - 1, y)) / (rho * deltaX)

  def calculate(n: Int) = {
    val t = n - 1
    var tempMap: Map[(Int, Int), Double] = u(t)
    // for even
    (1 to nX - 1).foreach(x =>
      (1 to nY - 1).foreach(y =>
        if ((t + x + y) % 2 == 0) {
          tempMap = tempMap.+((x, y) ->
            (u(t)(x, y) + tau * (f(x, y, t + 1) +
              nu * ((u(t)(x - 1, y) - 2 * u(t)(x, y) + u(t)(x + 1, y)) / (deltaX * deltaX) +
                (u(t)(x, y - 1) - 2 * u(t)(x, y) + u(t)(x, y + 1)) / (deltaY * deltaY)))))
        }
      )
    )

    // for odd
    (1 to nX - 1).foreach(x =>
      (1 to nY - 1).foreach(y =>
        if ((t + x + y) % 2 == 1) {
          tempMap = tempMap.+((x, y) ->
            (f(x, y, t + 1) + u(t)(x, y) / tau +
              nu * ((tempMap(x - 1, y) + tempMap(x + 1, y)) / (deltaX * deltaX) + (tempMap(x, y - 1) + tempMap(x, y + 1))/ (deltaY * deltaY))) /
              (1 / tau + 2 * nu / (deltaX * deltaX) + 2 * nu / (deltaY * deltaY)))
        }
      )
    )
    u = u.+((t + 1) -> tempMap)
  }

  def graph: (Map[Int, List[(Int, Int, Double)]], Map[Int, List[(Int, Int, Double)]]) = functionToGraph(u)

  def graphU: (Map[Int, List[(Int, Int, Double)]], Map[Int, List[(Int, Int, Double)]]) = {
    var graph1 = Map[Int, List[(Int, Int, Double)]]()
    var graph2 = Map[Int, List[(Int, Int, Double)]]()
    (0 to nT).foreach(t => {
      var list1: List[(Int, Int, Double)] = List()
      var list2: List[(Int, Int, Double)] = List()
      (0 to nX).foreach(x =>
        (0 to nY).foreach(y =>
          list1 = list1.::((x, y, fU(x, y, t)))
        )
      )
      (0 to nY).foreach(y =>
        (0 to nX).foreach(x =>
          list2 = list2.::((x, y, fU(x, y, t)))
        )
      )
      graph1 = graph1.+(t -> list1)
      graph2 = graph2.+(t -> list2)
    })
    (graph1, graph2)
  }

  def paintGraph = Graph.paint(functionToGraph(u), graphU)
}
