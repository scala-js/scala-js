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

import scala.concurrent._

import java.io._
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import org.scalajs.linker.interface.{OutputDirectory, Report}
import org.scalajs.linker.interface.unstable.{OutputDirectoryImpl, OutputPatternsImpl, ReportImpl}
import org.scalajs.linker.standard.{ModuleSet, IOThrottler}
import org.scalajs.linker.standard.ModuleSet.ModuleID

private[backend] abstract class OutputWriter(output: OutputDirectory,
    config: LinkerBackendImpl.Config) {
  import OutputWriter.ByteArrayWriter

  private val outputImpl = OutputDirectoryImpl.fromOutputDirectory(output)
  private val moduleKind = config.commonConfig.coreSpec.moduleKind

  protected def writeModule(moduleID: ModuleID, jsFileWriter: Writer): Unit

  protected def writeModule(moduleID: ModuleID, jsFileWriter: Writer, sourceMapWriter: Writer): Unit

  def write(moduleSet: ModuleSet)(implicit ec: ExecutionContext): Future[Report] = {
    val ioThrottler = new IOThrottler(config.maxConcurrentWrites)

    def filesToRemove(seen: Iterable[String], reports: List[Report.Module]): Set[String] =
      seen.toSet -- reports.flatMap(r => r.jsFileName :: r.sourceMapName.toList)

    for {
      currentFiles <- outputImpl.listFiles()
      reports <- Future.traverse(moduleSet.modules) { m =>
        ioThrottler.throttle(writeModule(m.id))
      }
      _ <- Future.traverse(filesToRemove(currentFiles, reports)) { f =>
        ioThrottler.throttle(outputImpl.delete(f))
      }
    } yield {
      val publicModules = for {
        (module, report) <- moduleSet.modules.zip(reports)
        if module.public
      } yield {
        report
      }

      new ReportImpl(publicModules)
    }
  }

  private def writeModule(moduleID: ModuleID)(
      implicit ec: ExecutionContext): Future[Report.Module] = {
    val jsFileName = OutputPatternsImpl.jsFile(config.outputPatterns, moduleID.id)

    if (config.sourceMap) {
      val sourceMapFileName = OutputPatternsImpl.sourceMapFile(config.outputPatterns, moduleID.id)

      val codeWriter = new ByteArrayWriter
      val smWriter = new ByteArrayWriter

      writeModule(moduleID, codeWriter.writer, smWriter.writer)

      val code = codeWriter.result()
      val sourceMap = smWriter.result()

      for {
        _ <- outputImpl.writeFull(jsFileName, code)
        _ <- outputImpl.writeFull(sourceMapFileName, sourceMap)
      } yield {
        new ReportImpl.ModuleImpl(moduleID.id, jsFileName, Some(sourceMapFileName), moduleKind)
      }
    } else {
      val codeWriter = new ByteArrayWriter

      writeModule(moduleID, codeWriter.writer)

      val code = codeWriter.result()

      for {
        _ <- outputImpl.writeFull(jsFileName, code)
      } yield {
        new ReportImpl.ModuleImpl(moduleID.id, jsFileName, None, moduleKind)
      }
    }
  }
}

private object OutputWriter {
  private class ByteArrayWriter {
    private val byteStream = new ByteArrayOutputStream

    val writer: Writer = new OutputStreamWriter(byteStream, StandardCharsets.UTF_8)

    def result(): ByteBuffer = {
      writer.close()
      ByteBuffer.wrap(byteStream.toByteArray())
    }
  }
}
