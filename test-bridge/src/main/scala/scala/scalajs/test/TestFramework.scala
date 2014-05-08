/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.test

import scala.scalajs.js
import js.annotation.{ JSExportDescendentObjects, JSExport }

/** This trait should be sub classed (as object) by a concrete test framework
 *
 *  It will receive a call to runTest for each object on the classpath
 *  extending Test
 */
@JSExportDescendentObjects
trait TestFramework {
  @JSExport
  final def safeRunTest(testOutput: TestOutput, args: js.Array[String])(
    test: js.Function0[Test]): Unit = {
    try {
      runTest(testOutput, args)(test)
    } catch {
      case e: Throwable =>
        testOutput.error(s"Test framework ${getClass.getName} failed:")
        testOutput.error(e.getMessage, e.getStackTrace)
    }
  }

  def runTest(testOutput: TestOutput, args: js.Array[String])(
    test: js.Function0[Test]): Unit
}
