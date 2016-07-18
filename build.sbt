name := "ScalaWorkshop"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
  "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.6.1"
)
