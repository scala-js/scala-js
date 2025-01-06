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

  import config.commonConfig.coreSpec

  require(
    coreSpec.moduleKind == ModuleKind.ESModule,
    s"The WebAssembly backend only supports ES modules; was ${coreSpec.moduleKind}."
  )
  require(
    coreSpec.esFeatures.useECMAScript2015Semantics,
    s"The WebAssembly backend only supports the ECMAScript 2015 semantics."
  )
  require(
    coreSpec.semantics.strictFloats,
    "The WebAssembly backend only supports strict float semantics."
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

    val outputImpl = OutputDirectoryImpl.fromOutputDirectory(output)

    val watFileName = s"$moduleID.wat"
    val wasmFileName = s"$moduleID.wasm"
    val sourceMapFileName = s"$wasmFileName.map"
    val jsFileName = OutputPatternsImpl.jsFile(config.outputPatterns, moduleID)

    val filesToProduce0 = Set(
      wasmFileName,
      loaderJSFileName,
      jsFileName
    )
    val filesToProduce1 =
      if (config.sourceMap) filesToProduce0 + sourceMapFileName
      else filesToProduce0
    val filesToProduce =
      if (config.prettyPrint) filesToProduce1 + watFileName
      else filesToProduce1

    def maybeWriteWatFile(): Future[Unit] = {
      if (config.prettyPrint) {
        val textOutput = TextWriter.write(wasmModule)
        val textOutputBytes = textOutput.getBytes(StandardCharsets.UTF_8)
        outputImpl.writeFull(watFileName, ByteBuffer.wrap(textOutputBytes))
      } else {
        Future.unit
      }
    }

    def writeWasmFile(): Future[Unit] = {
      val emitDebugInfo = !config.minify

      if (config.sourceMap) {
        val sourceMapWriter = new ByteArrayWriter

        val wasmFileURI = s"./$wasmFileName"
        val sourceMapURI = s"./$sourceMapFileName"

        val smWriter = new SourceMapWriter(sourceMapWriter, wasmFileURI,
            config.relativizeSourceMapBase, fragmentIndex)
        val binaryOutput = BinaryWriter.writeWithSourceMap(
            wasmModule, emitDebugInfo, smWriter, sourceMapURI)
        smWriter.complete()

        outputImpl.writeFull(wasmFileName, binaryOutput).flatMap { _ =>
          outputImpl.writeFull(sourceMapFileName, sourceMapWriter.toByteBuffer())
        }
      } else {
        val binaryOutput = BinaryWriter.write(wasmModule, emitDebugInfo)
        outputImpl.writeFull(wasmFileName, binaryOutput)
      }
    }

    def writeLoaderFile(): Future[Unit] =
      outputImpl.writeFull(loaderJSFileName, ByteBuffer.wrap(emitterResult.loaderContent))

    def writeJSFile(): Future[Unit] =
      outputImpl.writeFull(jsFileName, ByteBuffer.wrap(emitterResult.jsFileContent))

    for {
      existingFiles <- outputImpl.listFiles()
      _ <- Future.sequence(existingFiles.filterNot(filesToProduce).map(outputImpl.delete(_)))
      _ <- maybeWriteWatFile()
      _ <- writeWasmFile()
      _ <- writeLoaderFile()
      _ <- writeJSFile()
    } yield {
      val reportModule = new ReportImpl.ModuleImpl(
        moduleID,
        jsFileName,
        None,
        coreSpec.moduleKind
      )
      new ReportImpl(List(reportModule))
    }
  }
}
