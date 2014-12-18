package scala.tools.nsc

/* Super hacky overriding of the MainGenericRunner used by partest */

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.classpath._
import org.scalajs.core.tools.classpath.builder._
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.optimizer.ScalaJSOptimizer
import org.scalajs.core.tools.optimizer.ScalaJSClosureOptimizer
import org.scalajs.core.tools.optimizer.ParIncOptimizer

import org.scalajs.core.ir
import org.scalajs.core.ir.Utils.escapeJS

import org.scalajs.jsenv.JSConsole
import org.scalajs.jsenv.rhino.RhinoJSEnv
import org.scalajs.jsenv.nodejs.NodeJSEnv

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

  def readSemantics() = {
    val opt = sys.props.get("scalajs.partest.compliantSems")
    opt.fold(Semantics.Defaults) { str =>
      val sems = str.split(',')
      Semantics.compliantTo(sems.toList)
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
    val semantics   = readSemantics()

    def fastOpted = fastOptimize(classpath, mainObjName, logger, semantics)
    def fullOpted = fullOptimize(classpath, mainObjName, logger,
      baseRunner, semantics.optimized)

    val runner = {
      if (optMode == FullOpt)
        fullOptRunner()
      else
        baseRunner
    }

    val env =
      if (optMode == NoOpt) new RhinoJSEnv(semantics)
      else new NodeJSEnv

    val runClasspath = optMode match {
      case NoOpt         => classpath
      case FastOpt       => fastOpted
      case FullOpt       => fullOpted
    }

    env.jsRunner(runClasspath, runner, logger, jsConsole).run()

    true
  }

  private def runnerJSFile(mainObj: String, args: List[String]) = {
    val jsObj = "ScalaJS.m." + mainObj + "$"
    val jsArgs = argArray(args)
    new MemVirtualJSFile("Generated launcher file").
      withContent(s"$jsObj().main__AT__V($jsArgs);")
  }

  /** constructs a scala.Array[String] with the given elements */
  private def argArray(args: List[String]) = {
    val jsArgs = args.map(escapeJS).mkString("[\"", "\", \"", "\"]")
    s"""ScalaJS.makeNativeArrayWrapper(
          ScalaJS.d.T.getArrayOf(), $jsArgs)"""
  }

  private def fastOptimize(
      classpath: IRClasspath,
      mainObjName: String,
      logger: Logger,
      semantics: Semantics) = {
    import ScalaJSOptimizer._

    val optimizer = newScalaJSOptimizer(semantics)
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
    val mainModuleName = mainObjName + "$"
    List(
      ReachModule(mainModuleName),
      ReachMethod(mainModuleName, "main__AT__V", static = false)
    )
  }

  private def fullOptimize(
      classpath: IRClasspath,
      mainObjName: String,
      logger: Logger,
      runner: VirtualJSFile,
      semantics: Semantics) = {
    import ScalaJSClosureOptimizer._

    val fastOptimizer = newScalaJSOptimizer(semantics)
    val fullOptimizer = new ScalaJSClosureOptimizer(semantics)
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

  private def newScalaJSOptimizer(semantics: Semantics) =
    new ScalaJSOptimizer(semantics, new ParIncOptimizer(_))

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
