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

  def emit(unit: LinkingUnit, output: LinkerOutput, logger: Logger)(
      implicit ec: ExecutionContext): Future[Unit] = {

    val modules = importedModules(unit)
    Files.write(entryPointOutputFile, modules.toIterable.asJava,
        StandardCharsets.UTF_8)

    standard.emit(unit, output, logger)
  }

  private def importedModules(linkingUnit: LinkingUnit): List[String] = {
    def importedModulesOf(loadSpec: JSNativeLoadSpec): List[String] = {
      import JSNativeLoadSpec._
      loadSpec match {
        case Import(module, _)                              => List(module)
        case ImportWithGlobalFallback(Import(module, _), _) => List(module)
        case Global(_, _)                                   => Nil
      }
    }

    linkingUnit.classDefs
      .flatMap(_.jsNativeLoadSpec)
      .flatMap(importedModulesOf(_))
      .distinct
  }
}
