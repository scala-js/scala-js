package scala.scalajs.test

import sbt.testing.TaskDef
import sbt.testing.EventHandler
import sbt.testing.Task
import sbt.testing.Logger
import scala.scalajs.sbtplugin.ScalaJSEnvironment
import scala.annotation.tailrec
import org.mozilla.javascript.Context
import org.mozilla.javascript.Scriptable
import org.mozilla.javascript
import org.mozilla.javascript.Scriptable.NOT_FOUND
import org.mozilla.javascript.RhinoException
import scala.scalajs.sbtplugin.environment.rhino.CodeBlock
import scala.scalajs.sbtplugin.environment.rhino.Utilities

case class TestTask(
  environment: ScalaJSEnvironment,
  testRunnerClass: String, testFramework: String)(val taskDef: TaskDef) extends Task {

  val tags = Array.empty[String]

  def execute(eventHandler: EventHandler, loggers: Array[Logger]):Array[Task] = {
    val testKey = taskDef.fullyQualifiedName.replaceAll("\\.", "_")
    val testFrameworkKey = testFramework.replaceAll("\\.", "_")

    val eventProxy = EventProxy(eventHandler, loggers, new Events(taskDef))

    environment.runInContextAndScope { (context, scope) =>
      new CodeBlock(context, scope) with Utilities {

        val jsEventProxy = eventProxy

        try {
          createInstance(testRunnerClass, jsEventProxy, testFrameworkKey, testKey)
        } catch {
          case t: RhinoException =>
            eventProxy.error(t.details, t.getScriptStack())
        }
      }
    }

    Array.empty
  }
}
