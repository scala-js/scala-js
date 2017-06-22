addSbtPlugin("org.scala-js" % "sbt-scalajs" %
  org.scalajs.core.ir.ScalaJSVersions.current)

libraryDependencies +=
  "org.scala-js" %% "scalajs-jsdom-nodejs-env" % org.scalajs.core.ir.ScalaJSVersions.current
