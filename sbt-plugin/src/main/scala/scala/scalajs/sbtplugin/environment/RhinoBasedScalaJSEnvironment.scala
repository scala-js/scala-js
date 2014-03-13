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
import org.mozilla.javascript.Undefined

import rhino.ContextOps
import rhino.ScalaJSCoreLib
import rhino.ScriptableObjectOps

import sbt._

import scala.scalajs.sbtplugin.Utils._

class RhinoBasedScalaJSEnvironment(
    inputs: Seq[File], classpath: Seq[File],
    console: Option[Console],
    trace: (=> Throwable) => Unit) extends ScalaJSEnvironment {

  private val EncodedNameLine = raw""""encodedName": *"([^"]+)"""".r.unanchored

  /** ScalaJS core lib (with lazy-loading of Scala.js providers) and .js
   *  scripts to be executed immediately.
   */
  lazy val (scalaJSCoreLib, exportedScalaJSSymbols, immediateJSInputs) = {
    /* This splits the inputs in 3 categories:
     * 1. The scalajs-corejslib.js file
     * 2. Scala.js providers (.js files emitted by Scala.js)
     * 3. Other .js files that must be executed immediately
     */
    var scalaJSCoreLibFile: Option[File] = None
    val scalaJSProviders = mutable.Map.empty[String, File]
    val exportedScalaJSSymbols = mutable.ListBuffer.empty[String]
    val immediateJSInputs = mutable.ListBuffer.empty[File]

    inputs.foreach {
      case file @ CoreJSLibFile() =>
        scalaJSCoreLibFile = Some(file)

      case file @ ScalaJSClassFile(infoFile) =>
        val lines = IO.readLines(infoFile)
        val encodedName = lines.collectFirst {
          case EncodedNameLine(encodedName) => encodedName
        }.getOrElse {
          throw new AssertionError(s"Did not find encoded name in $infoFile")
        }
        scalaJSProviders += encodedName -> file
        val isExported = lines.exists(_ == """  "isExported": true,""")
        if (isExported)
          exportedScalaJSSymbols += encodedName

      case file =>
        immediateJSInputs += file
    }

    if (scalaJSCoreLibFile == None && scalaJSProviders.nonEmpty)
      throw new RuntimeException(
          "Could not find scalajs-corejslib.js, make sure it is on the classpath")
    val scalaJSCoreLib = scalaJSCoreLibFile.map(
        new ScalaJSCoreLib(_).withProviders(scalaJSProviders))

    (scalaJSCoreLib, exportedScalaJSSymbols.toList, immediateJSInputs.toList)
  }

  /** Additional .js scripts available on the classpath (for importScripts).
   *  Isolates the env.js library if available.
   */
  lazy val (envJSLib, availableJSFiles) = {
    val availableJSFiles = mutable.Map.empty[String, File]
    var envJSLib: Option[File] = None

    for (dir <- classpath.reverse) { // reverse so earlier dirs shadow later dirs
      for (file <- (dir ** "*.js").get) {
        file match {
          case CoreJSLibFile() | ScalaJSClassFile(_) =>
          case EnvJSLib() =>
            envJSLib = Some(file)
          case _ =>
            availableJSFiles += file.getName -> file
        }
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
        scalaJSCoreLib.foreach { lib =>
          lib.insertInto(context, scope)

          // Make sure exported symbols are loaded
          val ScalaJS = scope.get("ScalaJS", scope).asInstanceOf[Scriptable]
          val c = ScalaJS.get("c", ScalaJS).asInstanceOf[Scriptable]
          for (encodedName <- exportedScalaJSSymbols)
            c.get(encodedName, c)
        }

        // Then we execute immediate .js scripts
        for (file <- immediateJSInputs)
          context.evaluateFile(scope, file)

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
}
