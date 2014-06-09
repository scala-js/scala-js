/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.testing

import scala.scalajs.tools.env._
import scala.scalajs.tools.classpath._

import sbt._
import sbt.testing._
import sbt.classpath.ClasspathFilter

import java.net.URLClassLoader

class TestFramework(
    environment: JSEnv,
    jsConsole: JSConsole,
    testFramework: String) extends Framework {

  val name = "Scala.js Test Framework"

  lazy val fingerprints = Array[Fingerprint](f1)

  private val f1 = new SubclassFingerprint {
    val isModule = true
    val superclassName = "scala.scalajs.test.Test"
    val requireNoArgConstructor = true
  }

  def runner(args: Array[String], remoteArgs: Array[String],
      testClassLoader: ClassLoader): Runner = {

    val jsClasspath = extractClasspath(testClassLoader)
    new TestRunner(environment, jsClasspath, jsConsole,
      testFramework, args, remoteArgs)
  }

  /** extract classpath from ClassLoader (which must be a JSClasspathLoader) */
  private def extractClasspath(cl: ClassLoader) = cl match {
    case cl: JSClasspathLoader => cl.cp
    case _ =>
      sys.error("The Scala.js framework only works with a class loader of " +
          s"type JSClasspathLoader (${cl.getClass} given)")
  }

}
