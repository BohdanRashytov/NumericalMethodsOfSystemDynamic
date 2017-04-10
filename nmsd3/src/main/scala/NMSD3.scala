package nmsd3

import nmsd3.Common._

object NMSD3 {
  def main(args: Array[String]): Unit = {
    Common.init
    (1 to nT - 1).foreach(t => {
      PCalculator.calculate(t)
      UCalculator.calculate(t)
      VCalculator.calculate(t)
    })

    PCalculator.paintGraph
//    VCalculator.paintGraph
//    UCalculator.paintGraph
  }
}
