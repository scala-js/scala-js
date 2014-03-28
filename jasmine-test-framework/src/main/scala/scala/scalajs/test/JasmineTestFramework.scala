/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Framework    **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.test

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.JavaScriptException
import scala.scalajs.js.annotation.JSExport

object JasmineTestFramework extends TestFramework {

  /* Stub-out timer methods used by Jasmine and not provided by Rhino. */
  if (!global.setTimeout) {
    global.setTimeout = scalaJSStub("setTimeout")
    global.clearTimeout = scalaJSStub("clearTimeout")
    global.setInterval = scalaJSStub("setInterval")
    global.clearInterval = scalaJSStub("clearInterval")
  }

  def scalaJSStub(name: String): js.Function = { () =>
    global.console.log("Stub for " + name + " called")
  }

  // make sure jasmine is loaded
  global.importScripts("jasmine.js")

  def runTests(testOutput: TestOutput)(tests: js.Function0[Unit]): Unit = {
    val jasmine = global.jasmine
    val reporter = new JasmineTestReporter(testOutput)

    try {
      tests()

      val jasmineEnv = jasmine.getEnv()
      jasmineEnv.addReporter(reporter.asInstanceOf[js.Any])
      jasmineEnv.updateInterval = 0
      jasmineEnv.execute()
    } catch {
      case throwable@JavaScriptException(exception) =>
        testOutput.error("Problem executing code in tests: " + exception,
            throwable.getStackTrace())
    }
  }
}
