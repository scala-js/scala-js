package scala.tools.nsc

/* Super hacky overriding of the MainGenericRunner used by partest */

import scala.scalajs.sbtplugin.environment.{
  Console, RhinoBasedScalaJSEnvironment
}
import scala.scalajs.sbtplugin.environment.rhino.{
  CodeBlock, Utilities
}

import sbt._

import java.io.File
import Properties.{ versionString, copyrightString }
import GenericRunnerCommand._

class ConsoleConsole extends Console {
  def log(x: Any): Unit = scala.Console.out.println(x.toString)
  def info(x: Any): Unit = scala.Console.out.println(x.toString)
  def warn(x: Any): Unit = scala.Console.err.println(x.toString)
  def error(x: Any): Unit = scala.Console.err.println(x.toString)
}

class MainGenericRunner {
  def errorFn(ex: Throwable): Boolean = {
    ex.printStackTrace()
    false
  }
  def errorFn(str: String): Boolean = {
    scala.Console.err println str
    false
  }

  def process(args: Array[String]): Boolean = {
    val command = new GenericRunnerCommand(args.toList, (x: String) => errorFn(x))
    import command.{ settings, howToRun, thingToRun }

    if (!command.ok) return errorFn("\n" + command.shortUsageMsg)
    else if (settings.version) return errorFn("Scala code runner %s -- %s".format(versionString, copyrightString))
    else if (command.shouldStopWithInfo) return errorFn("shouldStopWithInfo")

    if (howToRun != AsObject)
      return errorFn("Scala.js runner can only run an object")

    val classpath: Seq[File] = for {
      url <- settings.classpathURLs
      file = urlToFile(url)
      if (file.isDirectory)
    } yield file

    val inputs = (classpath ** "*.js").get

    def trace(e: => Throwable): Unit = e.printStackTrace()

    val environment = new RhinoBasedScalaJSEnvironment(
        inputs, classpath, Some(new ConsoleConsole), trace)

    environment.runInContextAndScope { (context, scope) =>
      new CodeBlock(context, scope) with Utilities {
        callMainMethod(thingToRun, command.arguments.toArray)
      }
    }

    true
  }

  private def urlToFile(url: java.net.URL) = {
    try {
      new File(url.toURI())
    } catch {
      case e: java.net.URISyntaxException => new File(url.getPath())
    }
  }
}

object MainGenericRunner extends MainGenericRunner {
  def main(args: Array[String]) {
    if (!process(args))
      sys.exit(1)
  }
}
