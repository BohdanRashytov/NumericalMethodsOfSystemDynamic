package nmsd1

import nmsd1.Common._

object ChangeOfDirectionScheme {
  val function: Map[Int, Map[(Int, Int), Double]] = {

    var v: Map[Int, Map[(Int, Int), Double]] = initialConditionsForV
    t.foreach(t => if (t != 0) {
      var halfTimeMap: Map[(Int, Int), Double] = v(t)
      var tempMap: Map[(Int, Int), Double] = Map()

      // search for halfTimeMap
      (1 to N - 1).foreach { m =>
        val A: Double = -alpha / (hx * hx)
        val B: Double = A
        val C: Double = 2 / tau - 2 * A
        var alphaI: Map[Int, Double] = Map(0 -> -B / C)
        (1 to N).foreach(i => alphaI = alphaI.+(i -> -B / (A * alphaI(i - 1) + C)))
        val FI: Map[Int, Double] = (0 to N).map(k =>
          k -> (f(k, m, t - 1 / 2) + v(t - 1)(k, m) / (tau / 2) + alpha * (v(t - 1)(k, m - 1) -
            2 * v(t - 1)(k, m) + v(t - 1)(k, m + 1)) / (hy * hy))).toMap
        var betaI: Map[Int, Double] = Map(0 -> -FI(0) / C)
        (1 to N).foreach(i => betaI = betaI.+(i -> (FI(i - 1) - A * betaI(i - 1)) / (A * alphaI(i - 1) + C)))
        halfTimeMap = halfTimeMap.+((N, m) -> ((FI(N) - A * betaI(N)) / (C + A * alphaI(N))))

        (1 to N - 1).map(i => N - i).foreach(k =>
          halfTimeMap = halfTimeMap.+((k, m) -> (alphaI(k + 1) * halfTimeMap(k + 1, m) + betaI(k + 1)))
        )
      }
      //search for tempMap
      (1 to N - 1).foreach { k =>
        val A: Double = -alpha / (hy * hy)
        val B: Double = A
        val C: Double = 2 / tau - 2 * A
        var alphaI: Map[Int, Double] = Map(0 -> -B / C)
        (1 to N).foreach(i => alphaI = alphaI.+(i -> -B / (A * alphaI(i - 1) + C)))
        val FI: Map[Int, Double] = (0 to N).map(m =>
          m -> (f(k, m, t - 1 / 2) + halfTimeMap(k, m) / (tau / 2) + alpha * (halfTimeMap(k - 1, m) -
            2 * halfTimeMap(k, m) + halfTimeMap(k + 1, m)) / (hx * hx))).toMap
        var betaI: Map[Int, Double] = Map(0 -> -FI(0) / C)
        (1 to N).foreach(i => betaI = betaI.+(i -> (FI(i - 1) - A * betaI(i - 1)) / (A * alphaI(i - 1) + C)))
        tempMap = tempMap.+((k, N) -> ((FI(N) - A * betaI(N)) / (C + A * alphaI(N))))
        (1 to N).map(i => N - i).foreach(m =>
          tempMap = tempMap.+((k, m) -> (alphaI(m + 1) * tempMap(k, m + 1) + betaI(m + 1)))
        )
      }

      if (v.keys.toList.contains(t)) {
        tempMap = (v(t)).++(tempMap)
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
