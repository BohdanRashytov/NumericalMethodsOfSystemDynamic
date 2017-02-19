package nmsd1

import nmsd1.Common._

object ExplicitDifferenceScheme {
  val function: Map[Int, Map[(Int, Int), Double]] = {
    var v: Map[Int, Map[(Int, Int), Double]] = initialConditionsForV
    t.foreach(t => if (t != 0) {
      var tempMap: Map[(Int, Int), Double] = Map()
      for (k <- 1 to N - 1; m <- 1 to N - 1) {
        tempMap = tempMap.+((k, m) ->
          (f(k, m, t - 1) +
            v(t - 1)(k, m) / tau -
            alpha * (
              (v(t - 1)((k - 1), m) - 2 * v(t - 1)(k, m) + v(t - 1)((k + 1), m)) / (hx * hx) +
                (v(t - 1)(k, (m - 1)) - 2 * v(t - 1)(k, m) + v(t - 1)(k, (m + 1))) / (hy * hy))))
      }
      if (v.keys.toList.contains(t)) {
        tempMap = tempMap.++(v(t))
        v = v.+(t -> tempMap)
      } else {
        v = v.+(t -> tempMap)
      }
    })
    v
  }

  val graph: (Map[Int, List[(Int, Int, Double)]], Map[Int, List[(Int, Int, Double)]]) = functionToGraph(function)

  val errors: Map[Int, Double] = {
    var errors: Map[Int, Double] = Map()
    t.foreach(t => {
      var error: Double = 0.0
      x.foreach(x =>
        y.foreach(y =>
          error = error + Math.abs(w(x, y, t) - function(t)(x, y))
        )
      )
      errors = errors.+(t -> error)
    })
    errors
  }

  def paintGraph = Graph.paint(graph, errors)
}
