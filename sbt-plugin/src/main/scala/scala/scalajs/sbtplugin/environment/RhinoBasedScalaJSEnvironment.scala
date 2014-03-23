/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.environment

import java.io.File

import scala.collection.Map
import scala.collection.mutable
import scala.util.matching.Regex

import scala.scalajs.tools.io.{IO => _, _}
import scala.scalajs.tools.classpath._
import scala.scalajs.sbtplugin.ScalaJSEnvironment
import scala.scalajs.sbtplugin.sourcemap.SourceMappedException

import org.mozilla.javascript.Context
import org.mozilla.javascript.RhinoException
import org.mozilla.javascript.Scriptable
import org.mozilla.javascript.ScriptableObject
import org.mozilla.javascript.Undefined

import rhino.ContextOps
import rhino.ScalaJSCoreLib
import rhino.ScriptableObjectOps

import sbt._

import scala.scalajs.sbtplugin.Utils._

class RhinoBasedScalaJSEnvironment(
    classpath: ScalaJSClasspathEntries,
    console: Option[Console],
    trace: (=> Throwable) => Unit) extends ScalaJSEnvironment {

  /** Scala.js core lib (with lazy-loading of Scala.js providers). */
  lazy val scalaJSCoreLib =
    if (classpath.classFiles.isEmpty) None
    else Some(new ScalaJSCoreLib(classpath))

  /** Additional .js scripts available on the classpath (for importScripts).
   *  Isolates the env.js library if available.
   */
  lazy val (envJSLib, availableJSFiles) = {
    val availableJSFiles = mutable.Map.empty[String, VirtualJSFile]
    var envJSLib: Option[VirtualJSFile] = None

    val EnvJSLibNamePattern = """env\.rhino\.(?:[\d\.]+\.)?js""".r

    for (file <- classpath.otherJSFiles) {
      file.name match {
        case EnvJSLibNamePattern(_*) =>
          envJSLib = Some(file)
        case _ =>
          availableJSFiles += file.name -> file
      }
    }

    (envJSLib, availableJSFiles)
  }

  /** Executes code in an environment where the Scala.js library is set up to
   *  load its classes lazily.
   *
   *  Other .js scripts in the inputs are executed eagerly before the provided
   *  `code` is called.
   *
   *  Additional .js files available on the classpath can be loaded using the
   *  importScripts JavaScript function.
   */
  def runInContextAndScope(code: (Context, ScriptableObject) => Unit): Unit = {
    val context = Context.enter()
    try {
      val scope = context.initStandardObjects()

      // Make sure Rhino does not do its magic for JVM top-level packages (#364)
      for (name <- Seq("java", "scala", "com", "org"))
        ScriptableObject.putProperty(scope, name, Undefined.instance)

      envJSLib.foreach { file =>
        context.setOptimizationLevel(-1)
        //please do not print the envjs header
        scope.addFunction("print", args => ())
        context.evaluateFile(scope, file)
        //add the real print function
        scope.addFunction("print", print(console))
      }

      console.foreach { console =>
        ScriptableObject.putProperty(scope, "console",
          Context.javaToJS(console, scope))
      }

      scope.addFunction("importScripts",
          importScripts(context, scope, availableJSFiles))

      try {
        // Make the core lib available
        scalaJSCoreLib.foreach(_.insertInto(context, scope))

        code(context, scope)
      } catch {
        case e: RhinoException =>
          trace(e) // print the stack trace while we're in the Context
          throw new SourceMappedException(e)

        case e: Exception =>
          trace(e) // print the stack trace while we're in the Context
          throw new RuntimeException("Exception while running JS code", e)
      }
    } finally {
      Context.exit()
    }
  }

  private def print(console: Option[Console])(args: Array[AnyRef]) =
    console.foreach(_.log(args.mkString(" ")))

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
}
