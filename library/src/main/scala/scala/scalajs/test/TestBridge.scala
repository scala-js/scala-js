package scala.scalajs.test

import scala.scalajs.js.Dynamic.newInstance
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js
import scala.scalajs.js.JavaScriptException

class TestBridge(reporter: EventProxy, framework: String, test: String) {

  val testOutput = new TestOutputBridge(reporter)

  val testFramework =
    global.ScalaJS.modules.applyDynamic(framework)()
      .asInstanceOf[TestFramework]

  testFramework.runTests(testOutput) {
    global.ScalaJS.modules.applyDynamic(test)()
  }
}
