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
import scala.collection.mutable

import java.io._
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.util.Arrays

import org.scalajs.ir.SHA1

import org.scalajs.linker.interface.{OutputDirectory, Report}
import org.scalajs.linker.interface.unstable.{OutputDirectoryImpl, OutputPatternsImpl, ReportImpl}
import org.scalajs.linker.standard.{ModuleSet, IOThrottler}
import org.scalajs.linker.standard.ModuleSet.ModuleID

private[backend] abstract class OutputWriter(output: OutputDirectory,
    config: LinkerBackendImpl.Config, skipContentCheck: Boolean) {
  import OutputWriter._

  private val outputImpl = OutputDirectoryImpl.fromOutputDirectory(output)
  private val moduleKind = config.commonConfig.coreSpec.moduleKind

  protected def writeModuleWithoutSourceMap(moduleID: ModuleID, force: Boolean): Option[ByteBuffer]

  protected def writeModuleWithSourceMap(moduleID: ModuleID, force: Boolean): Option[(ByteBuffer,
      ByteBuffer)]

  def write(moduleSet: ModuleSet)(implicit ec: ExecutionContext): Future[Report] = {
    val ioThrottler = new IOThrottler(config.maxConcurrentWrites)

    def filesToRemove(seen: Set[String], reports: List[Report.Module]): Set[String] =
      seen -- reports.flatMap(r => r.jsFileName :: r.sourceMapName.toList)

    for {
      currentFilesList <- outputImpl.listFiles()
      currentFiles = currentFilesList.toSet
      reports <- {
        if (config.contentHash) {
          writeModulesWithContentHash(moduleSet, ioThrottler)
        } else {
          Future.traverse(moduleSet.modules) { m =>
            ioThrottler.throttle(writeModule(m.id, currentFiles))
          }
        }
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

  private def writeModulesWithContentHash(moduleSet: ModuleSet, ioThrottler: IOThrottler)(
      implicit ec: ExecutionContext): Future[List[Report.Module]] = {

    // Step 1: Collect all module content (force = true to always get content).
    val moduleContents: Map[ModuleID, Either[Array[Byte], (Array[Byte], Array[Byte])]] = {
      moduleSet.modules.map { m =>
        val content: Either[Array[Byte], (Array[Byte], Array[Byte])] = {
          if (config.sourceMap) {
            writeModuleWithSourceMap(m.id, force = true) match {
              case Some((js, sm)) => Right((toByteArray(js), toByteArray(sm)))
              case None           =>
                throw new IllegalStateException(
                    s"Module ${m.id.id} produced no content despite force=true")
            }
          } else {
            writeModuleWithoutSourceMap(m.id, force = true) match {
              case Some(js) => Left(toByteArray(js))
              case None     =>
                throw new IllegalStateException(
                    s"Module ${m.id.id} produced no content despite force=true")
            }
          }
        }
        m.id -> content
      }.toMap
    }

    // Step 2: Sort modules topologically (dependencies before their dependents).
    val sortedModules = topologicalSort(moduleSet.modules)

    // Maps from ModuleID to its computed hash-based module ID string.
    val hashBasedModuleIDs = mutable.Map.empty[ModuleID, String]

    // Step 3: Process modules in topological order, computing hashes.
    val finalContents =
      mutable.Map.empty[ModuleID, Either[Array[Byte], (Array[Byte], Array[Byte])]]

    for (m <- sortedModules) {
      val moduleID = m.id

      moduleContents(moduleID) match {
        case Left(jsBytes) =>
          // No source map: substitute dependency names then compute hash.
          val updatedJS = substituteDependencyNames(jsBytes, m, hashBasedModuleIDs)
          val hash = computeContentHash(updatedJS)
          hashBasedModuleIDs(moduleID) = s"${moduleID.id}.$hash"
          finalContents(moduleID) = Left(updatedJS)

        case Right((jsBytes, smBytes)) =>
          // With source map: substitute dependency names, compute hash from
          // updated JS, then update sourceMappingURL and source map's file field.
          val jsAfterDeps = substituteDependencyNames(jsBytes, m, hashBasedModuleIDs)
          val hash = computeContentHash(jsAfterDeps)
          val hashBasedModuleID = s"${moduleID.id}.$hash"
          hashBasedModuleIDs(moduleID) = hashBasedModuleID

          val origSMURI = OutputPatternsImpl.sourceMapURI(config.outputPatterns, moduleID.id)
          val hashSMURI = OutputPatternsImpl.sourceMapURI(config.outputPatterns, hashBasedModuleID)
          val origJSURI = OutputPatternsImpl.jsFileURI(config.outputPatterns, moduleID.id)
          val hashJSURI = OutputPatternsImpl.jsFileURI(config.outputPatterns, hashBasedModuleID)

          val finalJS = replaceAllBytes(jsAfterDeps,
              s"//# sourceMappingURL=$origSMURI\n".getBytes(StandardCharsets.UTF_8),
              s"//# sourceMappingURL=$hashSMURI\n".getBytes(StandardCharsets.UTF_8))
          val finalSM = replaceAllBytes(smBytes,
              origJSURI.getBytes(StandardCharsets.UTF_8),
              hashJSURI.getBytes(StandardCharsets.UTF_8))

          finalContents(moduleID) = Right((finalJS, finalSM))
      }
    }

    // Step 4: Write all files and build reports.
    Future.traverse(moduleSet.modules) { m =>
      val moduleID = m.id
      val hashBasedModuleID = hashBasedModuleIDs(moduleID)
      val jsFileName = OutputPatternsImpl.jsFile(config.outputPatterns, hashBasedModuleID)

      ioThrottler.throttle {
        finalContents(moduleID) match {
          case Left(jsBytes) =>
            val report = new ReportImpl.ModuleImpl(moduleID.id, jsFileName, None, moduleKind)
            outputImpl.writeFull(jsFileName, ByteBuffer.wrap(jsBytes), skipContentCheck)
              .map(_ => report: Report.Module)

          case Right((jsBytes, smBytes)) =>
            val smFileName =
              OutputPatternsImpl.sourceMapFile(config.outputPatterns, hashBasedModuleID)
            val report =
              new ReportImpl.ModuleImpl(moduleID.id, jsFileName, Some(smFileName), moduleKind)
            for {
              _ <- outputImpl.writeFull(jsFileName, ByteBuffer.wrap(jsBytes), skipContentCheck)
              _ <- outputImpl.writeFull(smFileName, ByteBuffer.wrap(smBytes), skipContentCheck)
            } yield {
              report: Report.Module
            }
        }
      }
    }
  }

  /** Substitutes the module names of internal dependencies with their
   *  hash-based equivalents in the given JS bytes.
   */
  private def substituteDependencyNames(jsBytes: Array[Byte], m: ModuleSet.Module,
      hashBasedModuleIDs: scala.collection.Map[ModuleID, String]): Array[Byte] = {
    var result = jsBytes
    for (dep <- m.internalDependencies) {
      val origModuleName = OutputPatternsImpl.moduleName(config.outputPatterns, dep.id)
      val hashBasedModuleName =
        OutputPatternsImpl.moduleName(config.outputPatterns, hashBasedModuleIDs(dep))
      if (origModuleName != hashBasedModuleName) {
        result = replaceAllBytes(result,
            origModuleName.getBytes(StandardCharsets.UTF_8),
            hashBasedModuleName.getBytes(StandardCharsets.UTF_8))
      }
    }
    result
  }

  private def writeModule(moduleID: ModuleID, existingFiles: Set[String])(
      implicit ec: ExecutionContext): Future[Report.Module] = {
    val jsFileName = OutputPatternsImpl.jsFile(config.outputPatterns, moduleID.id)

    if (config.sourceMap) {
      val sourceMapFileName = OutputPatternsImpl.sourceMapFile(config.outputPatterns, moduleID.id)
      val report =
        new ReportImpl.ModuleImpl(moduleID.id, jsFileName, Some(sourceMapFileName), moduleKind)
      val force = !existingFiles.contains(jsFileName) || !existingFiles.contains(sourceMapFileName)

      writeModuleWithSourceMap(moduleID, force) match {
        case Some((code, sourceMap)) =>
          for {
            _ <- outputImpl.writeFull(jsFileName, code, skipContentCheck)
            _ <- outputImpl.writeFull(sourceMapFileName, sourceMap, skipContentCheck)
          } yield {
            report
          }
        case None =>
          Future.successful(report)
      }
    } else {
      val report = new ReportImpl.ModuleImpl(moduleID.id, jsFileName, None, moduleKind)
      val force = !existingFiles.contains(jsFileName)

      writeModuleWithoutSourceMap(moduleID, force) match {
        case Some(code) =>
          for {
            _ <- outputImpl.writeFull(jsFileName, code, skipContentCheck)
          } yield {
            report
          }
        case None =>
          Future.successful(report)
      }
    }
  }
}

private[backend] object OutputWriter {

  /** Computes a SHA-1-based content hash string (16 hex characters). */
  def computeContentHash(bytes: Array[Byte]): String = {
    val digestBuilder = new SHA1.DigestBuilder
    digestBuilder.update(bytes)
    val hashBytes = digestBuilder.finalizeDigest()
    val sb = new StringBuilder(16)
    var i = 0
    while (i < 8) {
      val b = hashBytes(i) & 0xff
      if (b < 0x10) sb.append('0')
      sb.append(Integer.toHexString(b))
      i += 1
    }
    sb.toString
  }

  /** Returns the content of a [[ByteBuffer]] as a new byte array. */
  def toByteArray(buf: ByteBuffer): Array[Byte] = {
    val arr = new Array[Byte](buf.remaining())
    buf.duplicate().get(arr)
    arr
  }

  /** Replaces all (non-overlapping) occurrences of `from` in `data` with `to`.
   *
   *  If `from` is empty or equal to `to`, returns `data` unchanged.
   */
  def replaceAllBytes(data: Array[Byte], from: Array[Byte], to: Array[Byte]): Array[Byte] = {
    // scalastyle:off return
    if (from.isEmpty || Arrays.equals(from, to)) return data

    val out = new ByteArrayOutputStream(data.length)
    var i = 0
    while (i < data.length) {
      if (matchesAt(data, i, from)) {
        out.write(to)
        i += from.length
      } else {
        out.write(data(i).toInt)
        i += 1
      }
    }
    out.toByteArray
    // scalastyle:on return
  }

  private def matchesAt(data: Array[Byte], pos: Int, pattern: Array[Byte]): Boolean = {
    // scalastyle:off return
    if (pos + pattern.length > data.length) return false
    var i = 0
    while (i < pattern.length) {
      if (data(pos + i) != pattern(i)) return false
      i += 1
    }
    true
    // scalastyle:on return
  }

  /** Sorts modules topologically so that each module appears after all of its
   *  internal dependencies. This guarantees that when computing a module's
   *  content hash, all dependency hashes are already known.
   */
  def topologicalSort(modules: List[ModuleSet.Module]): List[ModuleSet.Module] = {
    val moduleMap = modules.map(m => m.id -> m).toMap
    val visited = mutable.Set.empty[ModuleID]
    val result = mutable.ListBuffer.empty[ModuleSet.Module]

    def visit(m: ModuleSet.Module): Unit = {
      if (!visited(m.id)) {
        visited += m.id
        for (dep <- m.internalDependencies)
          moduleMap.get(dep).foreach(visit)
        result += m
      }
    }

    modules.foreach(visit)
    result.toList
  }
}
