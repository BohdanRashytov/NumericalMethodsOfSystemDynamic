package nmsd2

import java.lang.Math._

object Common {
  // time
  val h = 1

  // a+b+c=1, a=0.5
  val a = 0.5
  val b = 0.4
  val c = 0.1

  // 0<p(i)<1
  var p = List(
    0.5,
    0.2,
    1.0,
    1.0,
    0.12,
    0.13,
    0.38,
    0.15,
    1.0,
    0.75,
    0.75
  )

  def randomPi = p = (1 to 11).map(_ => random()).toList

  val t = (24 to 96).toList

  var A: Map[Int, Double] = Map()
  var G1: Map[Int, Double] = Map()
  var S: Map[Int, Double] = Map()
  var G2M: Map[Int, Double] = Map()
  var Ne: Map[Int, Double] = Map()

  def calcA(t: Int, termG1: Double, termS: Double, termG2M: Double, termA: Double): Double =
    p(6 - 1) * (G1(t) + termG1) + p(5 - 1) * (S(t) + termS) + b * p(4 - 1) * (G2M(t) + termG2M) - p(11 - 1) * (A(t) + termA)

  def calcG1(t: Int, termG2M: Double, termG1: Double): Double =
    a * p(3 - 1) * (G2M(t) + termG2M) - (p(6 - 1) + p(1 - 1) + p(7 - 1)) * (G1(t) + termG1)

  def calcS(t: Int, termG1: Double, termS: Double): Double =
    p(1 - 1) * (G1(t) + termG1) - (p(5 - 1) + p(8 - 1) + p(2 - 1)) * (S(t) + termS)

  def calcG2M(t: Int, termS: Double, termG2M: Double): Double =
    p(2 - 1) * (S(t) + termS) - (a * p(3 - 1) + b * p(4 - 1) + c * p(9 - 1)) * (G2M(t) + termG2M)

  def calcNe(t: Int, termG1: Double, termS: Double, termG2M: Double, termNe: Double): Double =
    p(7 - 1) * (G1(t) + termG1) + p(8 - 1) * (S(t) + termS) + c * p(9 - 1) * (G2M(t) + termG2M) - p(10 - 1) * (Ne(t) + termNe)

  def init() = {
    A = A.+(t(0) -> random())
    G1 = G1.+(t(0) -> random())
    S = S.+(t(0) -> random())
    G2M = G2M.+(t(0) -> random())
    Ne = Ne.+(t(0) -> random())
  }

  def normalization(t: Int) = {
    val sum = A(t) + G1(t) + S(t) + G2M(t) + Ne(t)
    A = A.+(t -> A(t) / sum)
    G1 = G1.+(t -> G1(t) / sum)
    S = S.+(t -> S(t) / sum)
    G2M = G2M.+(t -> G2M(t) / sum)
    Ne = Ne.+(t -> Ne(t) / sum)
  }

  def calculated() = {
    t.foreach(t => {
      if (t == Common.t(0)) {
        init()
        normalization(t)
      } else {
        //randomPi
        val k1A = calcA(t - 1, 0, 0, 0, 0)
        val k1G1 = calcG1(t - 1, 0, 0)
        val k1S = calcS(t - 1, 0, 0)
        val k1G2M = calcG2M(t - 1, 0, 0)
        val k1Ne = calcNe(t - 1, 0, 0, 0, 0)

        val k2A = calcA(t - 1, k1G1 / 2, k1S / 2, k1G2M / 2, k1A / 2)
        val k2G1 = calcG1(t - 1, k1G2M / 2, k1G1 / 2)
        val k2S = calcS(t - 1, k1G1 / 2, k1S / 2)
        val k2G2M = calcG2M(t - 1, k1S / 2, k1G2M / 2)
        val k2Ne = calcNe(t - 1, k1G1 / 2, k1S / 2, k1G2M / 2, k1Ne / 2)

        val k3A = calcA(t - 1, k2G1 / 2, k2S / 2, k2G2M / 2, k2A / 2)
        val k3G1 = calcG1(t - 1, k2G2M / 2, k2G1 / 2)
        val k3S = calcS(t - 1, k2G1 / 2, k2S / 2)
        val k3G2M = calcG2M(t - 1, k2S / 2, k2G2M / 2)
        val k3Ne = calcNe(t - 1, k2G1 / 2, k2S / 2, k2G2M / 2, k2Ne / 2)

        val k4A = calcA(t - 1, k3G1, k3S, k3G2M, k3A)
        val k4G1 = calcG1(t - 1, k3G2M, k3G1)
        val k4S = calcS(t - 1, k3G1, k3S)
        val k4G2M = calcG2M(t - 1, k3S, k3G2M)
        val k4Ne = calcNe(t - 1, k3G1, k3S, k3G2M, k3Ne)

        A = A.+(t -> (A(t - 1) + (k1A + 2 * k2A + 2 * k3A + k4A) / 6))
        G1 = G1.+(t -> (G1(t - 1) + (k1G1 + 2 * k2G1 + 2 * k3G1 + k4G1) / 6))
        S = S.+(t -> (S(t - 1) + (k1S + 2 * k2S + 2 * k3S + k4S) / 6))
        G2M = G2M.+(t -> (G2M(t - 1) + (k1G2M + 2 * k2G2M + 2 * k3G2M + k4G2M) / 6))
        Ne = Ne.+(t -> (Ne(t - 1) + (k1Ne + 2 * k2Ne + 2 * k3Ne + k4Ne) / 6))
      }
    })
  }
}
