package scala.tools.nsc

/* Super hacky overriding of the MainGenericRunner used by partest */

import scala.scalajs.tools.classpath._
import scala.scalajs.tools.logging._
import scala.scalajs.tools.io._
import scala.scalajs.tools.env.JSConsole

import scala.scalajs.sbtplugin.env.rhino.RhinoJSEnv
import scala.scalajs.sbtplugin.JSUtils._

import java.io.File
import Properties.{ versionString, copyrightString }
import GenericRunnerCommand._

class ScalaConsoleLogger extends Logger {
  def log(level: Level, message: =>String): Unit = {
    if (level == Level.Warn || level == Level.Error)
      scala.Console.err.println(message)
    else
      scala.Console.out.println(message)
  }
  def success(message: => String): Unit = info(message)
  def trace(t: => Throwable): Unit = t.printStackTrace()
}

class ScalaConsoleJSConsole extends JSConsole {
  def log(msg: Any) = scala.Console.out.println(msg.toString)
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

    val usefulClasspathEntries = for {
      url <- settings.classpathURLs
      f = urlToFile(url)
      if (f.isDirectory || f.getName.startsWith("scalajs-library"))
    } yield f
    val classpath = ScalaJSClasspath.fromClasspath(usefulClasspathEntries)

    val env = new RhinoJSEnv
    val logger = new ScalaConsoleLogger
    val jsConsole = new ScalaConsoleJSConsole

    env.runJS(
      classpath,
      runnerJSFile(thingToRun, command.arguments),
      logger,
      jsConsole)

    true
  }

  private def runnerJSFile(mainObj: String, args: List[String]) = {
    val jsObj = "ScalaJS.modules." + mainObj.replace('.', '_')
    val jsArgs = listToJS(args)
    new MemVirtualJSFile("Generated launcher file").
      withContent(s"$jsObj().main__AT__V($jsArgs);")
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
