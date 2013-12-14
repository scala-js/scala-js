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

class RhinoBasedScalaJSEnvironment(
    inputs: Seq[File], classpath: Seq[File],
    console: Option[Console],
    trace: (=> Throwable) => Unit) extends ScalaJSEnvironment {

  val ScalaJSProviderPattern = """[0-9]{4}-(.*\.js)""".r

  /* Splits the inputs into 4 categories
   *
   * 1. The core lib (using lazy loading for Scala.js files on the classpath)
   * 2. Other js files and
   * 3. Scala.js files (not on the classpath)
   * 4. Env.js lib if it is present
   */
  lazy val (scalaJSCoreLib, availableJsFiles, scalaJSFiles, envJSLib) = {
    var scalaJSCoreLib: Option[ScalaJSCoreLib] = None
    val scalaJSProviders, availableJsFiles = mutable.Map.empty[String, File]
    val scalaJSFiles = mutable.Seq.empty[File]
    var envJSLib: Option[File] = None

    inputs.foreach {
      case ScalaJSCore(lib) =>
        scalaJSCoreLib = Some(lib.withProviders(scalaJSProviders))

      case file @ RelativeScalaJSProvider(relativeFileName) =>
        scalaJSProviders += relativeFileName -> file

      case file @ ScalaJSProvider() =>
        scalaJSFiles :+ file

      case file @ EnvJSLib() =>
        envJSLib = Some(file)

      case file =>
        availableJsFiles += file.getName -> file
    }

    if (scalaJSCoreLib == None && (scalaJSProviders.nonEmpty | scalaJSFiles.nonEmpty))
      throw new RuntimeException("Could not find scalajs-corejslib.js, make sure it's on the classpath")

    (scalaJSCoreLib, availableJsFiles, scalaJSFiles, envJSLib)
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

      scope.addFunction("importScripts", importScripts(context, scope, availableJsFiles))

      try {
        // We only need to make the core lib available
        scalaJSCoreLib.foreach(_.insertInto(context, scope))

        // Any Scala.js files that were not on the classpath need to be loaded
        scalaJSFiles.foreach(context.evaluateFile(scope, _))

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
  private val ScalaJSProvider = new FileNameMatcher(ScalaJSProviderPattern)

  private object ScalaJSCore {
    def unapply(file: File) =
      if (file.getName == "scalajs-corejslib.js")
        Some(new ScalaJSCoreLib(file))
      else None
  }

  private object RelativeScalaJSProvider {
    def unapply(file: File): Option[String] = {
      val classpath = classpathOf(file)

      (file.getName, classpath) match {
        case (ScalaJSProviderPattern(fileName), Some(classpath)) =>
          Some(makeRelative(fileName, classpath))
        case _ => None
      }
    }

    private def makeRelative(fileName: String, classpath: String) = {
      val rel = classpath.replace("\\", "/")
      val lastSlash = rel.lastIndexOf("/")
      val relativeFileName = rel.substring(0, lastSlash + 1) + fileName
      relativeFileName
    }

    private def classpathOf(file: File) = {
      classpath.view
        .map(IO.relativize(_, file))
        .collectFirst {
          case Some(path) => path
        }
    }
  }
}
