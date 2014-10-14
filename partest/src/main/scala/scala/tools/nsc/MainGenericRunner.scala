package scala.tools.nsc

/* Super hacky overriding of the MainGenericRunner used by partest */

import scala.scalajs.ir

import scala.scalajs.tools.classpath._
import scala.scalajs.tools.classpath.builder._
import scala.scalajs.tools.logging._
import scala.scalajs.tools.io._
import scala.scalajs.tools.optimizer.ScalaJSOptimizer
import scala.scalajs.tools.optimizer.ScalaJSClosureOptimizer
import scala.scalajs.tools.optimizer.ParIncOptimizer
import scala.scalajs.tools.env.JSConsole

import scala.scalajs.sbtplugin.env.rhino.RhinoJSEnv
import scala.scalajs.sbtplugin.env.nodejs.NodeJSEnv
import scala.scalajs.sbtplugin.JSUtils._

import scala.tools.partest.scalajs.ScalaJSPartestOptions._

import java.io.File
import scala.io.Source

import Properties.{ versionString, copyrightString }
import GenericRunnerCommand._

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

  val optMode = OptMode.fromId(sys.props("scalajs.partest.optMode"))

  def noWarnMissing = {
    import ScalaJSOptimizer._

    for {
      fname <- sys.props.get("scalajs.partest.noWarnFile").toList
      line  <- Source.fromFile(fname).getLines
      if !line.startsWith("#")
    } yield line.split('.') match {
      case Array(className) =>             NoWarnClass(className)
      case Array(className, methodName) => NoWarnMethod(className, methodName)
    }
  }

  def process(args: Array[String]): Boolean = {
    val command = new GenericRunnerCommand(args.toList, (x: String) => errorFn(x))
    import command.{ settings, howToRun, thingToRun }

    if (!command.ok) return errorFn("\n" + command.shortUsageMsg)
    else if (settings.version) return errorFn("Scala code runner %s -- %s".format(versionString, copyrightString))
    else if (command.shouldStopWithInfo) return errorFn("shouldStopWithInfo")

    if (howToRun != AsObject)
      return errorFn("Scala.js runner can only run an object")

    // Load basic Scala.js classpath (used for running or further packaging)
    val usefulClasspathEntries = for {
      url <- settings.classpathURLs
      f = urlToFile(url)
      if (f.isDirectory || f.getName.startsWith("scalajs-library"))
    } yield f
    val classpath =
      PartialClasspathBuilder.build(usefulClasspathEntries).resolve()

    val logger    = new ScalaConsoleLogger(Level.Warn)
    val jsConsole = new ScalaConsoleJSConsole

    val mainObjName = ir.Definitions.encodeClassName(thingToRun)
    val baseRunner  = runnerJSFile(mainObjName, command.arguments)

    def fastOpted = fastOptimize(classpath, mainObjName, logger)
    def fullOpted = fullOptimize(classpath, mainObjName, logger, baseRunner)

    val runner = {
      if (optMode == FullOpt)
        fullOptRunner()
      else
        baseRunner
    }

    val env = if (optMode == NoOpt) new RhinoJSEnv else new NodeJSEnv
    val runClasspath = optMode match {
      case NoOpt         => classpath
      case FastOpt       => fastOpted
      case FullOpt       => fullOpted
    }

    env.runJS(runClasspath, runner, logger, jsConsole)

    true
  }

  private def runnerJSFile(mainObj: String, args: List[String]) = {
    val jsObj = "ScalaJS.m." + mainObj
    val jsArgs = argArray(args)
    new MemVirtualJSFile("Generated launcher file").
      withContent(s"$jsObj().main__AT__V($jsArgs);")
  }

  /** constructs a scala.Array[String] with the given elements */
  private def argArray(args: List[String]) = {
    s"""ScalaJS.makeNativeArrayWrapper(
          ScalaJS.d.T.getArrayOf(),
          ${listToJS(args)})"""
  }

  private def fastOptimize(
      classpath: IRClasspath,
      mainObjName: String,
      logger: Logger) = {
    import ScalaJSOptimizer._

    val optimizer = newScalaJSOptimizer
    val output = WritableMemVirtualJSFile("partest fastOpt file")

    optimizer.optimizeCP(
        Inputs(classpath,
            manuallyReachable = fastOptReachable(mainObjName),
            noWarnMissing = noWarnMissing
        ),
        OutputConfig(
            output        = output,
            wantSourceMap = false,
            checkIR       = true
            ),
        logger)
  }

  private def fastOptReachable(mainObjName: String) = {
    import ScalaJSOptimizer._
    List(
      ReachObject(mainObjName),
      ReachMethod(mainObjName + '$', "main__AT__V", static = false)
    )
  }

  private def fullOptimize(
      classpath: IRClasspath,
      mainObjName: String,
      logger: Logger,
      runner: VirtualJSFile) = {
    import ScalaJSClosureOptimizer._

    val fastOptimizer = newScalaJSOptimizer
    val fullOptimizer = new ScalaJSClosureOptimizer
    val output = WritableMemVirtualJSFile("partest fullOpt file")
    val exportFile = fullOptExportFile(runner)

    fullOptimizer.optimizeCP(fastOptimizer, Inputs(
      input = ScalaJSOptimizer.Inputs(
        classpath,
        manuallyReachable = fastOptReachable(mainObjName),
        noWarnMissing = noWarnMissing),
      additionalExports = exportFile :: Nil),
      OutputConfig(
        output,
        checkIR = true,
        wantSourceMap = false),
    logger)

  }

  private def newScalaJSOptimizer =
    new ScalaJSOptimizer(() => new ParIncOptimizer)

  /** generates an exporter statement for the google closure compiler that runs
   *  what the normal test would
   */
  private def fullOptExportFile(runnerFile: VirtualJSFile) = {
    new MemVirtualJSFile("partest fullOpt exports").withContent(
      s"""this["runFullOptPartest"] = function() { ${runnerFile.content} };"""
    )
  }

  private def fullOptRunner() = new MemVirtualJSFile("partest fullOpt runner").
    withContent("runFullOptPartest();")

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
