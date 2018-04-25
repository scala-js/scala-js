package scala.tools.nsc

/* Super hacky overriding of the MainGenericRunner used by partest */

import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.io.IRFileCache.IRContainer
import org.scalajs.core.tools.linker._

import org.scalajs.core.ir

import org.scalajs.jsenv.JSConsole
import org.scalajs.jsenv.nodejs.NodeJSEnv

import scala.tools.partest.scalajs.ScalaJSPartestOptions._

import java.io.File
import java.net.URL
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

  val optMode = OptMode.fromId(System.getProperty("scalajs.partest.optMode"))

  def readSemantics() = {
    val opt = Option(System.getProperty("scalajs.partest.compliantSems"))
    opt.fold(Semantics.Defaults) { str =>
      val sems = str.split(',')
      Semantics.compliantTo(sems.toList)
    }
  }

  def process(args: Array[String]): Boolean = {
    val command = new GenericRunnerCommand(args.toList, (x: String) => errorFn(x))

    if (!command.ok) return errorFn("\n" + command.shortUsageMsg)
    else if (command.settings.version) return errorFn("Scala code runner %s -- %s".format(versionString, copyrightString))
    else if (command.shouldStopWithInfo) return errorFn("shouldStopWithInfo")

    if (command.howToRun != AsObject)
      return errorFn("Scala.js runner can only run an object")

    val logger = new ScalaConsoleLogger(Level.Warn)
    val jsConsole = new ScalaConsoleJSConsole
    val semantics0 = readSemantics()
    val semantics = if (optMode == FullOpt) semantics0.optimized else semantics0
    val ir = loadIR(command.settings.classpathURLs)

    val moduleInitializers = Seq(ModuleInitializer.mainMethodWithArgs(
        command.thingToRun, "main", command.arguments))

    val linkerConfig = StandardLinker.Config()
      .withCheckIR(true)
      .withSemantics(semantics)
      .withSourceMap(false)
      .withOptimizer(optMode != NoOpt)
      .withClosureCompiler(optMode == FullOpt)
      .withBatchMode(true)

    val linker = StandardLinker(linkerConfig)

    val sjsCode = {
      val output = WritableMemVirtualJSFile("partest.js")
      linker.link(ir, moduleInitializers, output, logger)
      output
    }

    new NodeJSEnv().jsRunner(sjsCode).run(logger, jsConsole)

    true
  }

  private def loadIR(classpathURLs: Seq[URL]) = {
    // FIXME We drop the “jrt:/packages” element which can not be converted to a file
    val irContainers =
      IRContainer.fromClasspath(classpathURLs.filter(_.toString != "jrt:/packages").map(urlToFile))
    val cache = (new IRFileCache).newCache
    cache.cached(irContainers)
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
  def main(args: Array[String]): Unit = {
    if (!process(args))
      System.exit(1)
  }
}
