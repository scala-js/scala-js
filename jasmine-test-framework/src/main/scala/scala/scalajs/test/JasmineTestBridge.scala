/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Framework    **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.test

import scala.scalajs.js
import js.Dynamic.global

object JasmineTestBridge extends TestBridge {
  def run(reporter: EventProxy, framework: String, test: String): Unit = {
    val testOutput = new TestOutputBridge(reporter)

    val testFramework = {
      val accessor = (global /: framework.split("\\."))(_.selectDynamic(_))
      accessor().asInstanceOf[TestFramework]
    }

    testFramework.runTests(testOutput) {
      val accessor = (global /: test.split("\\."))(_.selectDynamic(_))
      accessor()
    }
  }
}
