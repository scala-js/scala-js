/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.test

import scala.scalajs.js
import js.Dynamic.{ newInstance, global }
import js.JavaScriptException

class TestBridge(reporter: EventProxy, framework: String, test: String) {
  val testOutput = new TestOutputBridge(reporter)

  val testFramework =
    global.ScalaJS.modules.applyDynamic(framework)()
      .asInstanceOf[TestFramework]

  testFramework.runTests(testOutput) {
    global.ScalaJS.modules.applyDynamic(test)()
  }
}
