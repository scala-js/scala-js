/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.testing

import scala.scalajs.tools.environment._
import scala.scalajs.tools.classpath._

import sbt._

import sbt.testing.Fingerprint
import sbt.testing.Framework
import sbt.testing.SubclassFingerprint
import sbt.testing.Runner

import sbt.classpath.ClasspathFilter

import java.net.URLClassLoader

class TestFramework(
    environment: ScalaJSEnvironment,
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

    val classpath = classLoader2Classpath(testClassLoader)

    // TODO abstract what kind of classpath to create.
    val jsClasspath = ScalaJSClasspath.readEntriesInClasspath(classpath)

    new TestRunner(environment, jsClasspath, testFramework, args, remoteArgs)
  }

  /** extract (supsected) classpath from a ClassLoader since we cannot use
   *  a ClassLoader to load JS files
   */
  private def classLoader2Classpath(cl: ClassLoader): Seq[File] = cl match {
    case cl: URLClassLoader =>
      cl.getURLs().map(url => new File(url.toURI())).toList
    case sbtFilter: ClasspathFilter =>
      classLoader2Classpath(sbtFilter.getParent())
    case _ =>
      sys.error("You cannot use a Scala.js framework with a class loader of " +
          s"type: ${cl.getClass()}.")
  }

}
