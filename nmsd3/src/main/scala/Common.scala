package nmsd3

import java.lang.Math._

object Common {

  //границы области
  val a = PI
  val d = 1.0

  //сетка
  val nX = 30
  val nY = 30
  val nT = 500

  //шаг по координатам
  val deltaX = a / (nX)
  val deltaY = d / (nY)

  val beta = deltaX / deltaY

  //шаг по времени
  val tau = 0.001

  //какие-то параметры
  val rho = 1.0
  val nu = 1.0
  val Re = 1.0 / (rho * nu)
  val omega = 3.0 / 2.0 // [1,2]

  var u = Map[Int, Map[(Int, Int), Double]]()

  var v = Map[Int, Map[(Int, Int), Double]]()

  var p = Map[Int, Map[(Int, Int), Double]]()

  val initVal = init

  def init: Unit = {
    //init for time
    (1 to nT).foreach(t => {
      u = u + (t -> Map())
      v = v + (t -> Map())
      p = p + (t -> Map())
    })

    // t = 0
    (0 to 0).foreach(t => {
      var tempU = Map[(Int, Int), Double]()
      var tempV = Map[(Int, Int), Double]()
      var tempP = Map[(Int, Int), Double]()
      (0 to nY).foreach(y => {
        (0 to nX).foreach(x => {
          tempU = tempU + ((x, y) -> fU(x, y, 0))
          tempV = tempV + ((x, y) -> fV(x, y, 0))
          tempP = tempP + ((x, y) -> fP(x, y, 0))
        })
      })
      u = u + (0 -> tempU)
      v = v + (0 -> tempV)
      p = p + (0 -> tempP)
    })

    // x = 0, x = a
    (0 to nT).foreach(t => {
      var tempU = Map[(Int, Int), Double]()
      var tempV = Map[(Int, Int), Double]()
      var tempP = Map[(Int, Int), Double]()
      (0 to nY).foreach(y => {
        tempU = tempU + ((0, y) -> fU(0, y, t))
        tempU = tempU + ((nX, y) -> fU(nX, y, t))
        tempV = tempV + ((0, y) -> fV(0, y, t))
        tempV = tempV + ((nX, y) -> fV(nX, y, t))
        tempP = tempP + ((0, y) -> fP(0, y, t))
        tempP = tempP + ((nX, y) -> fP(nX, y, t))
      })
      u = u + (t -> tempU.++(u(t)))
      v = v + (t -> tempV.++(v(t)))
      p = p + (t -> tempP.++(p(t)))
    })

    // y = 0 y = d
    (0 to nT).foreach(t => {
      var tempU = Map[(Int, Int), Double]()
      var tempV = Map[(Int, Int), Double]()
      var tempP = Map[(Int, Int), Double]()
      (0 to nX).foreach(x => {
        tempU = tempU + ((x, 0) -> fU(x, 0, t))
        tempU = tempU + ((x, nY) -> fU(x, nY, t))
        tempV = tempV + ((x, 0) -> fV(x, 0, t))
        tempV = tempV + ((x, nY) -> fV(x, nY, t))
        tempP = tempP + ((x, 0) -> fP(x, 0, t))
        tempP = tempP + ((x, nY) -> fP(x, nY, t))
      })
      u = u + (t -> tempU.++(u(t)))
      v = v + (t -> tempV.++(v(t)))
      p = p + (t -> tempP.++(p(t)))
    })
  }

  def fU(x: Int, y: Int, t: Int) =
    -exp(-t * tau) * exp(2 * y * deltaY / d) * sin(2 * x * deltaX / d)

  def fV(x: Int, y: Int, t: Int) =
    exp(-t * tau) * exp(2 * y * deltaY / d) * cos(2 * x * deltaX / d)

  def fP(x: Int, y: Int, t: Int) =
    d / 2 * rho * exp(-t * tau) * exp(2 * y * deltaY / d) * cos(2 * x * deltaX / d)

  def functionToGraph(v: Map[Int, Map[(Int, Int), Double]]):
  (Map[Int, List[(Int, Int, Double)]], Map[Int, List[(Int, Int, Double)]]) = {
    var graph1: Map[Int, List[(Int, Int, Double)]] = Map()
    var graph2: Map[Int, List[(Int, Int, Double)]] = Map()
    (0 to nT - 1).foreach(t => {
      var list1: List[(Int, Int, Double)] = List()
      var list2: List[(Int, Int, Double)] = List()
      (0 to nY).foreach(y =>
        (0 to nX).foreach(x =>
          list1 = list1.::((x, y, v(t)(x, y)))
        )
      )
      (0 to nX).foreach(x =>
        (0 to nY).foreach(y =>
          list2 = list2.::((x, y, v(t)(x, y)))
        )
      )
      graph1 = graph1.+(t -> list1)
      graph2 = graph2.+(t -> list2)
    })
    (graph1, graph2)
  }
}
