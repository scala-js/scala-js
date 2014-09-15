/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.env.rhino

import scala.scalajs.tools.io._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.env._
import scala.scalajs.tools.logging._

import scala.io.Source

import org.mozilla.javascript._

class RhinoJSEnv(withDOM: Boolean = false) extends JSEnv {

  /** Executes code in an environment where the Scala.js library is set up to
   *  load its classes lazily.
   *
   *  Other .js scripts in the inputs are executed eagerly before the provided
   *  `code` is called.
   */
  def runJS(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole): Unit = {

    val context = Context.enter()
    try {
      val scope = context.initStandardObjects()

      // Make sure Rhino does not do its magic for JVM top-level packages (#364)
      for (name <- Seq("java", "scala", "com", "org"))
        ScriptableObject.putProperty(scope, name, Undefined.instance)

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

      // Setup console.log
      val jsconsole = context.newObject(scope)
      jsconsole.addFunction("log", _.foreach(console.log _))
      ScriptableObject.putProperty(scope, "console", jsconsole)

      try {
        // Make the classpath available. Either through lazy loading or by
        // simply inserting
        classpath match {
          case cp: CompleteIRClasspath =>
            // Setup lazy loading classpath and source mapper
            val optLoader = if (cp.scalaJSIR.nonEmpty) {
              val loader = new ScalaJSCoreLib(cp)

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
