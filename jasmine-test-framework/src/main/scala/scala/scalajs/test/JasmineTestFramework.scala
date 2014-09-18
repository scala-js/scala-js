/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Framework    **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.test

import scala.scalajs.js
import scala.scalajs.js.JavaScriptException
import scala.scalajs.js.annotation.JSExport

import org.scalajs.jasmine.Jasmine

import scala.collection.mutable

object JasmineTestFramework extends TestFramework {
  createStackPropertyOnThrowable()

  private def createStackPropertyOnThrowable(): Unit = {
    /* All Jasmine cares about when looking for stack trace data is a field
     * `stack` on the error object. Our Throwables do not have a `stack` field
     * because they are not subclasses of the JavaScript class Error.
     * However, a genuine Error object with the proper (lazy) stack field is
     * stored under the property stackdata by StackTrace.
     * This code installs a property getter on Throwable that will redirect
     * `throwable.stack` to `throwable.stackdata.stack` (when it exists).
     */

    val ThrowablePrototype = js.Object.getPrototypeOf(
        (new Throwable).asInstanceOf[js.Object]).asInstanceOf[js.Object]

    js.Object.defineProperty(ThrowablePrototype, "stack", js.Dynamic.literal(
        configurable = false,
        enumerable = false,
        get = { (self: js.Dynamic) =>
          self.stackdata && self.stackdata.stack
        }: js.ThisFunction
    ).asInstanceOf[js.PropertyDescriptor])
  }

  private val tags = mutable.Set.empty[String]

  def hasTag(tag: String): Boolean = tags.contains(tag)

  def runTest(testOutput: TestOutput, args: js.Array[String])(
    test: js.Function0[Test]): Unit = {

    val jasmine = Jasmine.jasmine
    val reporter = new JasmineTestReporter(testOutput)

    handleArgs(args, testOutput)

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
    } finally {
      clearTags()
    }
  }

  /** Set tags used in tests. Only use when invoking with HTML reporter */
  @JSExport
  def setTags(newTags: String*): Unit = {
    clearTags()
    tags ++= newTags
  }

  /** Clear tags used in tests. Only use when invoking with HTML reporter */
  @JSExport
  def clearTags(): Unit = tags.clear()

  private def handleArgs(args: js.Array[String], testOutput: TestOutput) = {
    for (arg <- args) {
      if (arg.startsWith("-t"))
        tags += arg.stripPrefix("-t")
      else
        testOutput.log.warn(s"Jasmine: Discarding argument: $arg")
    }
  }
}
