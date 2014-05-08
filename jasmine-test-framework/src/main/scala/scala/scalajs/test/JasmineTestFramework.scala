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
  def runTest(testOutput: TestOutput, args: js.Array[String])(
    test: js.Function0[Test]): Unit = {

    val jasmine = global.jasmine
    val reporter = new JasmineTestReporter(testOutput)

    if (args.length >= 1)
      testOutput.log.warn(s"Jasmine: Discarding arguments: $args")

    try {
      test()

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
