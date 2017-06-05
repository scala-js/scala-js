package scala.tools.nsc

/* Super hacky overriding of the MainGenericRunner used by partest */

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.linker.{Linker, ModuleInitializer}
import org.scalajs.core.tools.linker.backend.{OutputMode, ModuleKind}

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
    import org.scalajs.core.tools.sem.CheckedBehavior.Compliant

    val opt = Option(System.getProperty("scalajs.partest.compliantSems"))
    val compliantSems =
      opt.fold[List[String]](Nil)(_.split(',').toList.filter(_.nonEmpty))

    compliantSems.foldLeft(Semantics.Defaults) { (prev, compliantSem) =>
      compliantSem match {
        case "asInstanceOfs"         => prev.withAsInstanceOfs(Compliant)
        case "arrayIndexOutOfBounds" => prev.withArrayIndexOutOfBounds(Compliant)
        case "moduleInit"            => prev.withModuleInit(Compliant)
        case "strictFloats"          => prev.withStrictFloats(true)
      }
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
    val semantics = readSemantics()
    val ir = loadIR(command.settings.classpathURLs)

    val moduleInitializers = Seq(ModuleInitializer.mainMethodWithArgs(
        command.thingToRun, "main", command.arguments))

    val linkerConfig = Linker.Config()
      .withSourceMap(false)
      .withClosureCompiler(optMode == FullOpt)

    val linker = Linker(semantics, OutputMode.ECMAScript51Isolated,
        ModuleKind.NoModule, linkerConfig)

    val sjsCode = {
      val output = WritableMemVirtualJSFile("partest.js")
      linker.link(ir, moduleInitializers, output, logger)
      output
    }

    new NodeJSEnv().jsRunner(sjsCode :: Nil).run(logger, jsConsole)

    true
  }

  private def loadIR(classpathURLs: Seq[URL]) = {
    val irContainers =
      FileScalaJSIRContainer.fromClasspath(classpathURLs.map(urlToFile))
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
