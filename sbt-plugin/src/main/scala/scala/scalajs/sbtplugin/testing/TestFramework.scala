/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.testing

import scala.scalajs.sbtplugin.ScalaJSEnvironment
import sbt.testing.Fingerprint
import sbt.testing.Framework
import sbt.testing.SubclassFingerprint
import sbt.testing.Runner

class TestFramework(environment: ScalaJSEnvironment, testRunnerClass: String,
    testFramework: String) extends Framework {

  val name = "Scala.js Test Framework"

  lazy val fingerprints = Array[Fingerprint](f1)

  val f1 = new SubclassFingerprint {
    val isModule = true
    val superclassName = "scala.scalajs.test.Test"
    val requireNoArgConstructor = true
  }

  def runner(args: Array[String], remoteArgs: Array[String],
      testClassLoader: ClassLoader): Runner =
    TestRunner(args, remoteArgs, environment, testRunnerClass, testFramework)
}
