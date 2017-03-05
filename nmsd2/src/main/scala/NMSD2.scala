package nmsd2

import java.io.{BufferedWriter, FileWriter}
import java.lang.Math._
import nmsd2.Common._

object NMSD2 {

  val outputPath = "Output.txt"
  val writer = new FileWriter(outputPath)
  val bufferWriter = new BufferedWriter(writer)

  def main(args: Array[String]): Unit = {
    calculated()
    buildTable()
  }

  def buildTable() = {
    bufferWriter.write("t\t\t\tA%\t\t\t\tNe%\t\t\t\tG1%\t\t\t\tS%\t\t\t\tG2M%\n")
    t.foreach(t => {//if (t == 24 || t == 48 || t == 72 || t == 96) {
      normalization(t)
      val valA = format(A(t))
      val valNe = format(Ne(t))
      val valG1 = format(G1(t))
      val valS = format(S(t))
      val valG2M = format(G2M(t))
      bufferWriter.write(s"$t\t\t\t$valA\t\t\t$valNe\t\t\t$valG1\t\t\t$valS\t\t\t$valG2M\n")
    })
    bufferWriter.flush()
  }

  def format (x: Double, n: Int = 4) = (x*pow(10, n+2)).toInt*1.0/(1.0*pow(10,n))

}
