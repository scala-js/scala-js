/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Framework    **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jasminetest

import sbt.testing._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

final class JasmineRunner(
    private[jasminetest] val framework: JasmineFramework,
    val args: Array[String],
    val remoteArgs: Array[String],
    private[jasminetest] val classLoader: ClassLoader
) extends Runner {

  private[this] var isDone = false

  // This statement also ensures that
  // [[JasmineRunner.createStackPropertyOnThrowable]] has been called.
  JasmineRunner.handleArgs(args)

  def tasks(taskDefs: Array[TaskDef]): Array[Task] = {
    ensureNotDone()
    for (taskDef <- taskDefs)
      yield new JasmineTask(this, taskDef)
  }

  def done(): String = {
    ensureNotDone()
    isDone = true
    framework.runnerDone()
    ""
  }

  def receiveMessage(msg: String): Option[String] =
    throw new AssertionError("Received unexpected message")

  def serializeTask(task: Task, serializer: TaskDef => String): String = {
    ensureNotDone()
    serializer(task.taskDef)
  }

  def deserializeTask(task: String, deserializer: String => TaskDef): Task = {
    ensureNotDone()
    new JasmineTask(this, deserializer(task))
  }

  private def ensureNotDone(): Unit = {
    if (isDone)
      throw new IllegalStateException("Runner is done")
  }
}

object JasmineRunner {
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

  private def handleArgs(args: Array[String]): Unit = {
    val tags = for (arg <- args) yield {
      if (arg.startsWith("-t"))
        arg.stripPrefix("-t")
      else
        throw new IllegalArgumentException(
            s"Unknown argument for JasmineFramework: $arg")
    }

    TestSuiteContext.setTags(tags.toSet)
  }
}
