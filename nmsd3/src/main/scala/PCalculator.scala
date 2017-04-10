package nmsd3

import nmsd3.Common._

object PCalculator {

  def calculate(n: Int) = p = p + (n -> p(n).++({
    var tempMap = Map[(Int, Int), Double]()

    def temp(x: Int, y: Int) =
      if (x == 0 || x == nX || y == 0 || y == nY) fP(x, y, n)
      else tempMap(x, y)

    (1 to nY - 1).foreach(y => {
      (1 to nX - 1).foreach(x => {
        tempMap = tempMap + ((x, y) -> ((
          p(n - 1)(x, y) + (omega / (2 * (1 + beta) * (1 + beta))) * (
            p(n - 1)(x + 1, y) + temp(x - 1, y) +
              p(n - 1)(x, y + 1) * beta * beta + temp(x, y - 1) * beta * beta -
              tau * tau * S(n - 1)(x, y) - 2 * (1 + beta * beta) * p(n - 1)(x, y)
            )
          ))
          )
      })
    })
    tempMap
  }))

  def S(n: Int)(i: Int, j: Int) =
    D(n)(i, j) / tau + deltaD(n)(i, j) / Re

  def D(n: Int)(i: Int, j: Int) =
    (u(n)(i + 1, j) - u(n)(i - 1, j)) / (2 * deltaX) +
      (v(n)(i, j + 1) - v(n)(i, j - 1)) / (2 * deltaY)

  def deltaD(n: Int)(i: Int, j: Int) =
    (u(n)(i + 1, j) - 2 * u(n)(i, j) + u(n)(i - 1, j)) / (deltaX * deltaX) -
      (v(n)(i, j + 1) - 2 * v(n)(i, j) + v(n)(i, j - 1)) / (deltaY * deltaY)

  def graph: (Map[Int, List[(Int, Int, Double)]], Map[Int, List[(Int, Int, Double)]]) = functionToGraph(p)

  def graphP: (Map[Int, List[(Int, Int, Double)]], Map[Int, List[(Int, Int, Double)]]) = {
    var graph1 = Map[Int, List[(Int, Int, Double)]]()
    var graph2 = Map[Int, List[(Int, Int, Double)]]()
    (0 to nT).foreach(t => {
      var list1: List[(Int, Int, Double)] = List()
      var list2: List[(Int, Int, Double)] = List()
      (0 to nX).foreach(x =>
        (0 to nY).foreach(y =>
          list1 = list1.::((x, y, fP(x, y, t)))
        )
      )
      (0 to nY).foreach(y =>
        (0 to nX).foreach(x =>
          list2 = list2.::((x, y, fP(x, y, t)))
        )
      )
      graph1 = graph1.+(t -> list1)
      graph2 = graph2.+(t -> list2)
    })
    (graph1, graph2)
  }

  def paintGraph = Graph.paint(graph, graphP)

}
