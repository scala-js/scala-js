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
import org.scalajs.linker.standard.ModuleSet.ModuleID

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

  private val isWasmModule = coreSpec.moduleKind == ModuleKind.WasmModule

  def emit(moduleSet: ModuleSet, output: OutputDirectory, logger: Logger)(
      implicit ec: ExecutionContext): Future[Report] = {
    val emitterResult = emitter.emit(moduleSet, logger)

    val writerInputs = {
      emitterResult.loaderContent.map { content =>
        OutputWriter.OneFile(loaderJSFileName, true, () => ByteBuffer.wrap(content))
      }.iterator ++ emitterResult.body.iterator.flatMap {
        case (moduleID, emitterModule) => moduleWriterInput(moduleID, emitterModule)
      }
    }

    OutputWriter.write(writerInputs, output, config.maxConcurrentWrites,
        skipContentCheck = false).map(_ => report(moduleSet))
  }

  private def moduleWriterInput(moduleID: ModuleID,
      emitterModule: Emitter.Result.Module): Iterator[OutputWriter.Input] = {

    val wasmModule = emitterModule.wasmModule

    val watFileName = s"${moduleID.id}.wat"
    val wasmFileName = this.wasmFileName(moduleID)
    val sourceMapFileName = s"$wasmFileName.map"
    val jsFileName = OutputPatternsImpl.jsFile(config.outputPatterns, moduleID.id)

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

    val jsFileInput = emitterModule.jsFileContent.map { content =>
      OneFile(jsFileName, true, () => ByteBuffer.wrap(content))
    }

    maybeWat ++ Iterator(mainInput) ++ jsFileInput.iterator
  }

  private def report(moduleSet: ModuleSet): Report = {
    val publicModules = for {
      module <- moduleSet.modules if module.public
    } yield {
      val moduleFileName =
        if (isWasmModule) wasmFileName(module.id)
        else OutputPatternsImpl.jsFile(config.outputPatterns, module.id.id)

      new ReportImpl.ModuleImpl(module.id.id, moduleFileName, None, coreSpec.moduleKind)
    }

    new ReportImpl(publicModules)
  }

  private def wasmFileName(moduleID: ModuleID) = s"${moduleID.id}.wasm"
}
