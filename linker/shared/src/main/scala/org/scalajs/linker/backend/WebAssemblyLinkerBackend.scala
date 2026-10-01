/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.linker.backend

import scala.concurrent.{ExecutionContext, Future}

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import org.scalajs.logging.Logger

import org.scalajs.linker._
import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable._
import org.scalajs.linker.standard._

import org.scalajs.linker.backend.javascript.{ByteArrayWriter, SourceMapWriter}
import org.scalajs.linker.backend.webassembly._

import org.scalajs.linker.backend.wasmemitter.Emitter

final class WebAssemblyLinkerBackend(config: LinkerBackendImpl.Config)
    extends LinkerBackendImpl(config) {

  require(
    coreSpec.moduleKind == ModuleKind.ESModule ||
    coreSpec.moduleKind == ModuleKind.WasmModule,
    s"The WebAssembly backend only supports ESModule or WasmModule; " +
    s"was ${coreSpec.moduleKind}."
  )
  require(
    coreSpec.moduleKind != ModuleKind.ESModule ||
    coreSpec.esFeatures.esVersion >= ESVersion.ES2022,
    s"The WebAssembly backend requires ECMAScript 2022 or later for ESModule."
  )

  require(coreSpec.targetIsWebAssembly,
      s"A WebAssembly backend cannot be used with CoreSpec targeting JavaScript")

  val loaderJSFileName = OutputPatternsImpl.jsFile(config.outputPatterns, "__loader")

  private val fragmentIndex = new SourceMapWriter.Index

  private val emitter: Emitter = {
    val loaderModuleName = OutputPatternsImpl.moduleName(config.outputPatterns, "__loader")
    new Emitter(Emitter.Config(coreSpec, loaderModuleName))
  }

  val symbolRequirements: SymbolRequirement = emitter.symbolRequirements

  override def injectedIRFiles: Seq[IRFile] = emitter.injectedIRFiles

  def emit(moduleSet: ModuleSet, output: OutputDirectory, logger: Logger)(
      implicit ec: ExecutionContext): Future[Report] = {
    moduleSet.modules match {
      case Nil =>
        val outputImpl = OutputDirectoryImpl.fromOutputDirectory(output)
        for {
          currentFilesList <- outputImpl.listFiles()
          _ <- Future.traverse(currentFilesList) { f =>
            outputImpl.delete(f)
          }
        } yield new ReportImpl(Nil)
      case onlyModule :: Nil =>
        emit(onlyModule, moduleSet.globalInfo, output, logger)
      case modules =>
        throw new UnsupportedOperationException(
            "The WebAssembly backend does not support multiple modules. Found: " +
            modules.map(_.id.id).mkString(", "))
    }
  }

  private def emit(onlyModule: ModuleSet.Module, globalInfo: LinkedGlobalInfo,
      output: OutputDirectory, logger: Logger)(
      implicit ec: ExecutionContext): Future[Report] = {
    val moduleID = onlyModule.id.id

    val emitterResult = emitter.emit(onlyModule, globalInfo, logger)
    val wasmModule = emitterResult.wasmModule

    val watFileName = s"$moduleID.wat"
    val wasmFileName = s"$moduleID.wasm"
    val sourceMapFileName = s"$wasmFileName.map"
    val jsFileName = OutputPatternsImpl.jsFile(config.outputPatterns, moduleID)
    val isWasmModule = coreSpec.moduleKind == ModuleKind.WasmModule

    import OutputWriter.{OneFile, TwoFiles}

    val maybeWat = if (config.prettyPrint) {
      val file = OneFile(watFileName, true,
          () => {
            val textOutput = TextWriter.write(wasmModule)
            val textOutputBytes = textOutput.getBytes(StandardCharsets.UTF_8)
            ByteBuffer.wrap(textOutputBytes)
          })
      Iterator.single(file)
    } else {
      Iterator.empty
    }

    val emitDebugInfo = !config.minify

    val mainInput = if (config.sourceMap) {
      TwoFiles(wasmFileName, sourceMapFileName, true, () => {
        val sourceMapWriter = new ByteArrayWriter

        val wasmFileURI = s"./$wasmFileName"
        val sourceMapURI = s"./$sourceMapFileName"

        val smWriter = new SourceMapWriter(sourceMapWriter, wasmFileURI,
            config.relativizeSourceMapBase, fragmentIndex)
        val binaryOutput = BinaryWriter.writeWithSourceMap(
            wasmModule, emitDebugInfo, smWriter, sourceMapURI)
        smWriter.complete()

        (binaryOutput, sourceMapWriter.toByteBuffer())
      })
    } else {
      OneFile(wasmFileName, true,
          () => BinaryWriter.write(wasmModule, emitDebugInfo))
    }

    val loaderInput = emitterResult.loaderContent.map { content =>
      OneFile(loaderJSFileName, true, () => ByteBuffer.wrap(content))
    }

    val jsFileInput = emitterResult.jsFileContent.map { content =>
      OneFile(jsFileName, true, () => ByteBuffer.wrap(content))
    }

    val writerInputs =
      maybeWat ++ Iterator(mainInput) ++ loaderInput.iterator ++ jsFileInput.iterator

    val reportModule = new ReportImpl.ModuleImpl(
      moduleID,
      if (isWasmModule) wasmFileName else jsFileName,
      None,
      coreSpec.moduleKind
    )

    val report = new ReportImpl(List(reportModule))

    OutputWriter.write(writerInputs, output, config.maxConcurrentWrites,
        skipContentCheck = false).map(_ => report)
  }
}
