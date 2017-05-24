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

  val optMode = OptMode.fromId(sys.props("scalajs.partest.optMode"))

  def readSemantics() = {
    import org.scalajs.core.tools.sem.CheckedBehavior.Compliant

    val opt = sys.props.get("scalajs.partest.compliantSems")
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
    val ir = (
        loadIR(command.settings.classpathURLs) :+
        runnerIR(command.thingToRun, command.arguments)
    )

    val moduleInitializers =
      Seq(ModuleInitializer.mainMethod("PartestLauncher", "main"))

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

  private def runnerIR(mainObj: String, args: List[String]) = {
    import ir.Infos._
    import ir.ClassKind
    import ir.Trees._
    import ir.Types._

    val mainModuleClassName = ir.Definitions.encodeClassName(mainObj + "$")
    val className = "PartestLauncher$"
    val encodedClassName = ir.Definitions.encodeClassName(className)

    val definition = {
      implicit val DummyPos = ir.Position.NoPosition
      ClassDef(
        Ident(encodedClassName, Some(className)),
        ClassKind.ModuleClass,
        Some(Ident("O", Some("java.lang.Object"))),
        Nil,
        None,
        List(
          MethodDef(
            static = false,
            Ident("init___", Some("<init>")),
            Nil,
            NoType,
            Some(
              ApplyStatically(This()(ClassType(encodedClassName)),
                ClassType(ir.Definitions.ObjectClass),
                Ident("init___"),
                Nil
              )(NoType)
            )
          )(OptimizerHints.empty, None),
          MethodDef(
            static = false,
            Ident("main__V", Some("main")),
            Nil,
            NoType,
            Some(
              Apply(LoadModule(ClassType(mainModuleClassName)),
                Ident("main__AT__V"),
                List(
                  ArrayValue(ArrayType("T", 1), args.map(StringLiteral(_)))
                )
              )(NoType)
            )
          )(OptimizerHints.empty, None)
        )
      )(OptimizerHints.empty)
    }

    val info = generateClassInfo(definition)

    val infoAndDefinition = (info, definition)

    new VirtualScalaJSIRFile {
      def exists: Boolean = true
      def path: String = "PartestLauncher$.sjsir"
      def infoAndTree: (ClassInfo, ClassDef) = infoAndDefinition
    }
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
