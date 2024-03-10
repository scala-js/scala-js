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

import org.scalajs.linker.interface.{OutputDirectory, Report}
import org.scalajs.linker.interface.unstable.{OutputDirectoryImpl, OutputPatternsImpl, ReportImpl}
import org.scalajs.linker.standard.{ModuleSet, IOThrottler}
import org.scalajs.linker.standard.ModuleSet.ModuleID

import org.scalajs.linker.backend.javascript.ByteArrayWriter

private[backend] abstract class OutputWriter(output: OutputDirectory,
    config: LinkerBackendImpl.Config, skipContentCheck: Boolean) {

  private val outputImpl = OutputDirectoryImpl.fromOutputDirectory(output)
  private val moduleKind = config.commonConfig.coreSpec.moduleKind

  protected def moduleChanged(moduleID: ModuleID): Boolean

  protected def writeModuleWithoutSourceMap(moduleID: ModuleID): ByteBuffer

  protected def writeModuleWithSourceMap(moduleID: ModuleID): (ByteBuffer, ByteBuffer)

  def write(moduleSet: ModuleSet)(implicit ec: ExecutionContext): Future[Report] = {
    val ioThrottler = new IOThrottler(config.maxConcurrentWrites)

    def filesToRemove(seen: Set[String], reports: List[Report.Module]): Set[String] =
      seen -- reports.flatMap(r => r.jsFileName :: r.sourceMapName.toList)

    for {
      currentFilesList <- outputImpl.listFiles()
      currentFiles = currentFilesList.toSet
      reports <- Future.traverse(moduleSet.modules) { m =>
        ioThrottler.throttle(writeModule(m.id, currentFiles))
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

  private def writeModule(moduleID: ModuleID, existingFiles: Set[String])(
      implicit ec: ExecutionContext): Future[Report.Module] = {
    val jsFileName = OutputPatternsImpl.jsFile(config.outputPatterns, moduleID.id)

    if (config.sourceMap) {
      val sourceMapFileName = OutputPatternsImpl.sourceMapFile(config.outputPatterns, moduleID.id)
      val report = new ReportImpl.ModuleImpl(moduleID.id, jsFileName, Some(sourceMapFileName), moduleKind)

      if (moduleChanged(moduleID)) {
        val (code, sourceMap) = writeModuleWithSourceMap(moduleID)
        for {
          _ <- outputImpl.writeFull(jsFileName, code, skipContentCheck)
          _ <- outputImpl.writeFull(sourceMapFileName, sourceMap, skipContentCheck)
        } yield {
          report
        }
      } else {
        Future.successful(report)
      }
    } else {
      val report = new ReportImpl.ModuleImpl(moduleID.id, jsFileName, None, moduleKind)

      if (moduleChanged(moduleID)) {
        val code = writeModuleWithoutSourceMap(moduleID)
        for {
          _ <- outputImpl.writeFull(jsFileName, code, skipContentCheck)
        } yield {
          report
        }
      } else {
        Future.successful(report)
      }
    }
  }
}
