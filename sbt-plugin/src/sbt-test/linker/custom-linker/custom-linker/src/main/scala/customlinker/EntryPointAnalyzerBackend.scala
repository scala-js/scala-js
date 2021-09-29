package customlinker

import scala.concurrent.{ExecutionContext, Future}
import scala.collection.JavaConverters._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import org.scalajs.ir.Trees.JSNativeLoadSpec

import org.scalajs.logging.Logger

import org.scalajs.linker._
import org.scalajs.linker.interface._
import org.scalajs.linker.standard._

final class EntryPointAnalyzerBackend(linkerConfig: StandardConfig,
    entryPointOutputFile: Path)
    extends LinkerBackend  {

  require(linkerConfig.moduleKind != ModuleKind.NoModule,
      s"linkerConfig.moduleKind was ${linkerConfig.moduleKind}")

  private val standard = StandardLinkerBackend(linkerConfig)

  val coreSpec: CoreSpec = standard.coreSpec
  val symbolRequirements: SymbolRequirement = standard.symbolRequirements

  def injectedIRFiles: Seq[IRFile] = standard.injectedIRFiles

  def emit(moduleSet: ModuleSet, output: OutputDirectory, logger: Logger)(
      implicit ec: ExecutionContext): Future[Report] = {

    val modules = moduleSet.modules.flatMap(_.externalDependencies).toSet
    Files.write(entryPointOutputFile, (modules: Iterable[String]).asJava,
        StandardCharsets.UTF_8)

    standard.emit(moduleSet, output, logger)
  }
}
