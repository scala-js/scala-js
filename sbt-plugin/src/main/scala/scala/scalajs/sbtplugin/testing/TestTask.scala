/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.testing

import sbt.testing.TaskDef
import sbt.testing.EventHandler
import sbt.testing.Task
import sbt.testing.Logger
import scala.scalajs.sbtplugin.ScalaJSEnvironment
import scala.annotation.tailrec
import org.mozilla.javascript
import org.mozilla.javascript.{Context, Scriptable, Function}
import org.mozilla.javascript.Scriptable.NOT_FOUND
import org.mozilla.javascript.RhinoException
import scala.scalajs.sbtplugin.environment.rhino.CodeBlock
import scala.scalajs.sbtplugin.environment.rhino.Utilities

case class TestTask(
    environment: ScalaJSEnvironment,
    testRunnerClass: String,
    testFramework: String)(val taskDef: TaskDef) extends Task {

  val tags = Array.empty[String]

  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val testKey = taskDef.fullyQualifiedName
    val testFrameworkKey = testFramework

    val eventProxy = EventProxy(eventHandler, loggers, new Events(taskDef))

    environment.runInContextAndScope { (context, scope) =>
      new CodeBlock(context, scope) with Utilities {
        try {
          val f = context.evaluateString(scope,
              s"""var f = function(p, f, t) { $testRunnerClass().run(p, f, t); }; f;""",
              null, 0, null).asInstanceOf[Function]
          f.call(context, scope, null,
              toArgs(Seq(eventProxy, testFrameworkKey, testKey)))
        } catch {
          case t: RhinoException =>
            eventProxy.error(t.details, t.getScriptStack())
        }
      }
    }

    Array.empty
  }
}
