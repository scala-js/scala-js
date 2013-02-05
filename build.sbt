name := "scalajs"

version := "1.0"

scalaVersion := "2.10.0"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _)

mainClass := Some("scala.tools.jsc.Main")
