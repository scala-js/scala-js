/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.env.rhino

import scala.scalajs.tools.sem.Semantics
import scala.scalajs.tools.io._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.env._
import scala.scalajs.tools.logging._

import scala.io.Source

import org.mozilla.javascript._

class RhinoJSEnv(semantics: Semantics,
    withDOM: Boolean = false) extends AsyncJSEnv {

  /** Executes code in an environment where the Scala.js library is set up to
   *  load its classes lazily.
   *
   *  Other .js scripts in the inputs are executed eagerly before the provided
   *  `code` is called.
   */
  override def jsRunner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole): JSRunner = {
    new Runner(classpath, code, logger, console)
  }

  private class Runner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole) extends JSRunner {
    def run(): Unit = internalRunJS(classpath, code, logger, console)
  }

  override def asyncRunner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole): AsyncJSRunner = {
    new AsyncRunner(classpath, code, logger, console)
  }

  private class AsyncRunner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole) extends AsyncJSRunner {

    private[this] var resultThrowable: Throwable = null

    private[this] val thread = new Thread {
      override def run(): Unit = {
        try {
          internalRunJS(classpath, code, logger, console)
        } catch {
          case t: Throwable => resultThrowable = t
        }
      }
    }

    def start(): Unit = thread.start()

    def isRunning(): Boolean = thread.isAlive()

    def await(): Unit = {
      thread.join()
      if (resultThrowable != null)
        throw resultThrowable
    }
  }

  private def internalRunJS(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole): Unit = {

    val context = Context.enter()
    try {
      val scope = context.initStandardObjects()

      if (withDOM) {
        // Fetch env.rhino.js from webjar
        val name = "env.rhino.js"
        val path = "/META-INF/resources/webjars/envjs/1.2/" + name
        val resource = getClass.getResource(path)
        assert(resource != null, s"need $name as resource")

        // Rhino can't optimize envjs
        context.setOptimizationLevel(-1)

        // Don't print envjs header
        scope.addFunction("print", args => ())

        // Pipe file to Rhino
        val reader = Source.fromURL(resource).bufferedReader
        context.evaluateReader(scope, reader, name, 1, null);

        // No need to actually define print here: It is captured by envjs to
        // implement console.log, which we'll override in the next statement
      }

      // Make sure Rhino does not do its magic for JVM top-level packages (#364)
      val PackagesObject =
        ScriptableObject.getProperty(scope, "Packages").asInstanceOf[Scriptable]
      val topLevelPackageIds = ScriptableObject.getPropertyIds(PackagesObject)
      for (id <- topLevelPackageIds) (id: Any) match {
        case name: String => ScriptableObject.deleteProperty(scope, name)
        case index: Int   => ScriptableObject.deleteProperty(scope, index)
        case _            => // should not happen, I think, but with Rhino you never know
      }

      // Setup console.log
      val jsconsole = context.newObject(scope)
      jsconsole.addFunction("log", _.foreach(console.log _))
      ScriptableObject.putProperty(scope, "console", jsconsole)

      try {
        // Make the classpath available. Either through lazy loading or by
        // simply inserting
        classpath match {
          case cp: IRClasspath =>
            // Setup lazy loading classpath and source mapper
            val optLoader = if (cp.scalaJSIR.nonEmpty) {
              val loader = new ScalaJSCoreLib(semantics, cp)

              // Setup sourceMapper
              val scalaJSenv = context.newObject(scope)

              scalaJSenv.addFunction("sourceMapper", args => {
                val trace = Context.toObject(args(0), scope)
                loader.mapStackTrace(trace, context, scope)
              })

              ScriptableObject.putProperty(scope, "__ScalaJSEnv", scalaJSenv)

              Some(loader)
            } else {
              None
            }

            // Load JS libraries
            cp.jsLibs.foreach(lib => context.evaluateFile(scope, lib._1))

            optLoader.foreach(_.insertInto(context, scope))
          case cp =>
            cp.allCode.foreach(context.evaluateFile(scope, _))
        }

        context.evaluateFile(scope, code)
      } catch {
        case e: RhinoException =>
          // Trace here, since we want to be in the context to trace.
          logger.trace(e)
          sys.error(s"Exception while running JS code: ${e.getMessage}")
      }
    } finally {
      Context.exit()
    }
  }

}
