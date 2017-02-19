package nmsd1

import nmsd1.Common._


object DSFiniteDifferenceAlgorithm {
  val function: Map[Int, Map[(Int, Int), Double]] = {
    var v: Map[Int, Map[(Int, Int), Double]] = initialConditionsForV
    (0 to M - 1).foreach(t => {
      var tempMap: Map[(Int, Int), Double] = v(t)
      // for even
      (1 to N - 1).foreach(x =>
        (1 to N - 1).foreach(y =>
          if ((t + x + y) / 2 == 0) {
            tempMap = tempMap.+((x, y) ->
              (v(t)(x, y) + tau * (f(x, y, t + 1) +
                alpha * ((v(t)(x - 1, y) - 2 * v(t)(x, y) + v(t)(x + 1, y)) / (hx * hx) +
                  (v(t)(x, y - 1) - 2 * v(t)(x, y) + v(t)(x, y + 1)) / (hy * hy)))))
          }
        )
      )

      // for odd
      (1 to N - 1).foreach(x =>
        (1 to N - 1).foreach(y =>
          if ((t + x + y) / 2 == 1) {
            tempMap = tempMap.+((x, y) ->
              (f(x, y, t + 1) + v(t)(x, y) / tau +
                alpha * ((tempMap(x - 1, y) + tempMap(x + 1, y)) / (hx * hx) + (hy * hy) / ((tempMap(x, y - 1) + tempMap(x, y + 1))))) /
                (1 / tau + 2 * alpha / (hx * hx) + 2 * alpha / (hy * hy)))
          }
        )
      )
      v = v.+((t + 1) -> tempMap)
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
