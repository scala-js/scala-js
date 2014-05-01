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

import org.mozilla.javascript._

class RhinoJSEnv extends JSEnv {

  /** Executes code in an environment where the Scala.js library is set up to
   *  load its classes lazily.
   *
   *  Other .js scripts in the inputs are executed eagerly before the provided
   *  `code` is called.
   *
   *  Additional .js files available on the classpath can be loaded using the
   *  importScripts JavaScript function.
   */
  def runJS(classpath: JSClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole): Option[String] = {

    val context = Context.enter()
    try {
      val scope = context.initStandardObjects()

      // Make sure Rhino does not do its magic for JVM top-level packages (#364)
      for (name <- Seq("java", "scala", "com", "org"))
        ScriptableObject.putProperty(scope, name, Undefined.instance)

      // Extract envJS and load automatically
      val (envJSLib, availableJSFiles) = filterAvailable(classpath.otherJSFiles)

      envJSLib.foreach { file =>
        context.setOptimizationLevel(-1)
        //please do not print the envjs header
        scope.addFunction("print", args => ())
        context.evaluateFile(scope, file)
        //add the real print function
        scope.addFunction("print", print(console))
      }

      ScriptableObject.putProperty(scope, "console",
          Context.javaToJS(console, scope))

      scope.addFunction("importScripts",
          importScripts(context, scope, availableJSFiles))

      try {
        // Make the classpath available. Either through lazy loading or by
        // simply inserting
        classpath match {
          case cp: ScalaJSClasspath =>
            if (cp.irFiles.nonEmpty) {
              val loader = new ScalaJSCoreLib(cp)
              loader.insertInto(context, scope)
            }
          case _ =>
            classpath.mainJSFiles.foreach { f =>
              context.evaluateFile(scope, f)
            }
        }

        context.evaluateFile(scope, code)

        None
      } catch {
        case e: RhinoException =>
          logger.trace(e)
          Some(s"Exception while running JS code: ${e.getMessage}")
      }
    } finally {
      Context.exit()
    }
  }

  private def print(console: JSConsole)(args: Array[AnyRef]) =
    console.log(args.mkString(" "))

  private def importScripts(context: Context, globalScope: Scriptable,
      availableJSFiles: Map[String, VirtualJSFile])(args: Array[AnyRef]) = {
    val urls = args.map(_.toString)

    val files =
      for (url <- urls) yield {
        url -> availableJSFiles.get(url.split("/").last)
      }

    files.foreach {
      case (_, Some(file)) =>
        context.evaluateFile(globalScope, file)
      case (url, None) =>
        throw new RuntimeException("Could not find file: '" + url +
            "', make sure you make it available on any classpath")
    }
  }

  /** splits out the envJS library if available */
  private def filterAvailable(otherJSFiles: Seq[VirtualJSFile]) = {

    val envJSLibNamePat = """env\.rhino\.(?:[\d\.]+\.)?js""".r
    def isEnvJSLib(file: VirtualJSFile) =
      envJSLibNamePat.unapplySeq(file.name).isDefined

    val (envJSLibs, others) = otherJSFiles.partition(isEnvJSLib _)

    val availableJSFiles = others.map(f => (f.name, f)).toMap

    (envJSLibs.headOption, availableJSFiles)
  }

}
