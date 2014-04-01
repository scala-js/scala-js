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

@JSExportDescendentObjects
trait TestFramework {
  @JSExport
  def runTests(testOutput: TestOutput)(tests: js.Function0[Unit]): Unit
}
