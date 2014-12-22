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
      case Array(className) =>
        NoWarnMissing.Class(className)
      case Array(className, methodName) =>
        NoWarnMissing.Method(className, methodName)
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

    if (!command.ok) return errorFn("\n" + command.shortUsageMsg)
    else if (command.settings.version) return errorFn("Scala code runner %s -- %s".format(versionString, copyrightString))
    else if (command.shouldStopWithInfo) return errorFn("shouldStopWithInfo")

    if (command.howToRun != AsObject)
      return errorFn("Scala.js runner can only run an object")

    val logger = new ScalaConsoleLogger(Level.Warn)
    val jsConsole = new ScalaConsoleJSConsole
    val semantics = readSemantics()
    val classpath = createClasspath(command)

    val jsRunner = new MemVirtualJSFile("launcher.js")
      .withContent(s"PartestLauncher().launch();")

    val env =
      if (optMode == NoOpt) new RhinoJSEnv(semantics)
      else new NodeJSEnv

    val runClasspath = optMode match {
      case NoOpt   => classpath
      case FastOpt => fastOptimize(classpath, logger, semantics)
      case FullOpt => fullOptimize(classpath, logger, semantics.optimized)
    }

    env.jsRunner(runClasspath, jsRunner, logger, jsConsole).run()

    true
  }

  private def fastOptimize(classpath: IRClasspath,
      logger: Logger, semantics: Semantics) = {
    import ScalaJSOptimizer._

    val optimizer = newScalaJSOptimizer(semantics)
    val output = WritableMemVirtualJSFile("partest-fastOpt.js")

    optimizer.optimizeCP(
        classpath,
        Config(
            output        = output,
            wantSourceMap = false,
            checkIR       = true,
            noWarnMissing = noWarnMissing),
        logger)
  }

  private def fullOptimize(classpath: IRClasspath, logger: Logger,
      semantics: Semantics) = {
    import ScalaJSClosureOptimizer._

    val fastOptimizer = newScalaJSOptimizer(semantics)
    val fullOptimizer = new ScalaJSClosureOptimizer(semantics)
    val output = WritableMemVirtualJSFile("partest-fullOpt.js")

    fullOptimizer.optimizeCP(fastOptimizer,
        classpath, 
        Config(
          output,
          checkIR = true,
          wantSourceMap = false,
          noWarnMissing = noWarnMissing),
        logger)
  }

  private def createClasspath(command: GenericRunnerCommand) = {
    // Load basic Scala.js classpath (used for running or further packaging)
    val usefulClasspathEntries = for {
      url <- command.settings.classpathURLs
      f = urlToFile(url)
      if (f.isDirectory || f.getName.startsWith("scalajs-library"))
    } yield f

    val baseClasspath = PartialClasspathBuilder.build(usefulClasspathEntries)

    // Create a classpath with the launcher object
    val irFile = runnerIR(command.thingToRun, command.arguments)
    val launcherClasspath =
      new PartialClasspath(Nil, Map.empty, irFile :: Nil, None)

    (baseClasspath merge launcherClasspath).resolve()
  }

  private def runnerIR(mainObj: String, args: List[String]) = {
    import ir.Infos._
    import ir.ClassKind
    import ir.Trees._
    import ir.Types._

    val mainModuleClassName = ir.Definitions.encodeClassName(mainObj  + "$")
    val className = "PartestLauncher$"
    val exportName = "PartestLauncher"
    val encodedClassName = ir.Definitions.encodeClassName(className)

    val info = ClassInfo(
      encodedName = encodedClassName,
      kind = ClassKind.ModuleClass,
      isExported = true,
      superClass = "O",
      parents = List("O"),
      methods = List(
          MethodInfo("launch",
              isExported = true,
              accessedModules = List(mainModuleClassName),
              methodsCalled = Map(
                  mainModuleClassName -> List("main__AT__V")
              )
          )
      )
    )

    val definition = {
      implicit val DummyPos = ir.Position.NoPosition
      ClassDef(
        Ident(encodedClassName, Some(className)),
        ClassKind.ModuleClass,
        Some(Ident("O", Some("java.lang.Object"))),
        List(Ident("O", Some("java.lang.Object"))),
        List(
          MethodDef(
            static = false,
            StringLiteral("launch"),
            Nil,
            AnyType,
            Block(
              Apply(LoadModule(ClassType(mainModuleClassName)),
                Ident("main__AT__V"),
                List(ArrayValue(ArrayType("T", 1), args.map(StringLiteral(_))))
              )(NoType),
              Undefined()
            )
          )(OptimizerHints.empty, None),
          ModuleExportDef(exportName)
        )
      )(OptimizerHints.empty)
    }

    val infoAndDefinition = (info, definition)

    new VirtualScalaJSIRFile {
      def exists: Boolean = true
      def path: String = "PartestLauncher$.sjsir"
      def infoAndTree: (ClassInfo, ClassDef) = infoAndDefinition
    }
  }

  private def newScalaJSOptimizer(semantics: Semantics) =
    new ScalaJSOptimizer(semantics, new ParIncOptimizer(_))

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
