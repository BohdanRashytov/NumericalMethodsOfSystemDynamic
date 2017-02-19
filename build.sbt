name := "NumericalMethodsOfSystemDynamic"

version := "1.0"

scalaVersion := "2.12.1"

lazy val root = (project in file("."))
  .settings(
    name := "root",
    scalaVersion := "2.12.1",
    libraryDependencies += "com.github.yannrichet" % "JMathPlot" % "1.0.1" from "https://github.com/yannrichet/jmathplot/blob/master/dist/jmathplot.jar"
  )

lazy val nmsd1 = (project in file ("nmsd1"))
  .settings(
    libraryDependencies += "com.github.yannrichet" % "JMathPlot" % "1.0.1" from "https://github.com/yannrichet/jmathplot/blob/master/dist/jmathplot.jar"
  )
