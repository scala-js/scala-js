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
import scala.scalajs.sbtplugin.ScalaJSEnvironment
import scala.scalajs.sbtplugin.sourcemap.SourceMappedException
import scala.util.matching.Regex

import org.mozilla.javascript.Context
import org.mozilla.javascript.RhinoException
import org.mozilla.javascript.Scriptable
import org.mozilla.javascript.ScriptableObject

import rhino.ContextOps
import rhino.ScalaJSCoreLib
import rhino.ScriptableObjectOps

import sbt.IO

import scala.scalajs.sbtplugin.Utils._

class RhinoBasedScalaJSEnvironment(
    inputs: Seq[File], classpath: Seq[File],
    console: Option[Console],
    trace: (=> Throwable) => Unit) extends ScalaJSEnvironment {

  private val EncodedNameLine = raw""""encodedName": *"([^"]+)"""".r.unanchored

  /* Splits the inputs into 3 categories
   *
   * 1. The core lib (using lazy loading for Scala.js files)
   * 2. Other js files and
   * 3. Env.js lib if it is present
   */
  lazy val (scalaJSCoreLib, availableJSFiles, envJSLib) = {
    var scalaJSCoreLib: Option[ScalaJSCoreLib] = None
    val scalaJSProviders, availableJSFiles = mutable.Map.empty[String, File]
    var envJSLib: Option[File] = None

    inputs.foreach {
      case ScalaJSCore(lib) =>
        scalaJSCoreLib = Some(lib.withProviders(scalaJSProviders))

      case file @ ScalaJSClassFile(infoFile) =>
        val encodedName = IO.readLines(infoFile).collectFirst {
          case EncodedNameLine(encodedName) => encodedName
        }.getOrElse {
          throw new AssertionError(s"Did not find encoded name in $infoFile")
        }
        scalaJSProviders += encodedName -> file

      case file @ EnvJSLib() =>
        envJSLib = Some(file)

      case file =>
        availableJSFiles += file.getName -> file
    }

    if (scalaJSCoreLib == None && scalaJSProviders.nonEmpty)
      throw new RuntimeException("Could not find scalajs-corejslib.js, make sure it's on the classpath")

    (scalaJSCoreLib, availableJSFiles, envJSLib)
  }

  /** Executes code in an environment where the Scala.js library is set up to
   *  load its classes lazily.
   *
   *  Other js files can be loaded using the importScripts javascript
   *  function.
   */
  def runInContextAndScope(code: (Context, ScriptableObject) => Unit): Unit = {
    val context = Context.enter()
    try {
      val scope = context.initStandardObjects()

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

      scope.addFunction("importScripts", importScripts(context, scope, availableJSFiles))

      try {
        // We only need to make the core lib available
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
      availableJsFiles: Map[String, File])(args: Array[AnyRef]) = {
    val urls = args.map(_.toString)

    val files =
      for {
        url <- urls
        possibleName = url.split("/").lastOption
        name <- possibleName
        possibleFile = availableJsFiles.get(name)
        if possibleFile.forall(_.getAbsolutePath endsWith name)
      } yield url -> possibleFile

    files.foreach {
      case (_, Some(file)) =>
        context.evaluateFile(globalScope, file)
      case (url, None) =>
        throw new RuntimeException("Could not find file: '" + url +
            "', make sure you make it available on any classpath")
    }
  }

  private class FileNameMatcher(pattern: Regex) {
    def unapply(file: File): Boolean = {
      file.getName match {
        case pattern(_*) => true
        case _ => false
      }
    }
  }

  private val EnvJSLib = new FileNameMatcher("""env\.rhino\.(?:[\d\.]+\.)?js""".r)

  private object ScalaJSCore {
    def unapply(file: File) =
      if (isCoreJSLibFile(file)) Some(new ScalaJSCoreLib(file))
      else None
  }
}
