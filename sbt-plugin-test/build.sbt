import ScalaJSKeys._

import scala.scalajs.sbtplugin.RuntimeDOM

import scala.scalajs.sbtplugin.env.phantomjs.PhantomJSEnv

name := "Scala.js sbt test"

version := scalaJSVersion

val baseSettings = scalaJSSettings ++ Seq(
  version := scalaJSVersion,
  scalaVersion := "2.11.2",
  libraryDependencies +=
    "org.scala-lang.modules.scalajs" %% "scalajs-jasmine-test-framework" % scalaJSVersion % "test"
)

lazy val root = project.in(file(".")).aggregate(noDOM, withDOM)

lazy val noDOM = project.settings(baseSettings: _*).settings(
  name := "Scala.js sbt test w/o DOM"
)

lazy val withDOM = project.settings(baseSettings: _*).settings(
  name := "Scala.js sbt test w/ DOM",
  jsDependencies ++= Seq(
      RuntimeDOM,
      "org.webjars" % "jquery" % "1.10.2" / "jquery.js"
  )
)

lazy val jetty9 = project.settings(baseSettings: _*).settings(
  name := "Scala.js sbt test with jetty9 on classpath",
  jsDependencies ++= Seq(
      RuntimeDOM,
      "org.webjars" % "jquery" % "1.10.2" / "jquery.js"
  ),
  postLinkJSEnv := new PhantomJSEnv(
    addArgs = List("--web-security=no"), // allow cross domain requests
    jettyClassLoader = scalaJSPhantomJSClassLoader.value),
  Jetty9Test.runSetting
)
