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

import java.nio.ByteBuffer
import org.scalajs.linker.interface.OutputDirectory
import org.scalajs.linker.interface.unstable.OutputDirectoryImpl
import org.scalajs.linker.standard.IOThrottler

/** Handles cache and removal aware writing to an output directory. */
private[backend] final class OutputWriter private (
    outputImpl: OutputDirectoryImpl,
    prevFilesFuture: Future[Set[String]],
    maxConcurrentWrites: Int,
    skipContentCheck: Boolean
) {
  private val ioThrottler = new IOThrottler(maxConcurrentWrites)
  private val filesToKeep = mutable.Set.empty[String]
  private val opFutures = mutable.ArrayBuffer.empty[Future[_]]

  def write(fileNames: List[String], changed: Boolean = true)(
      writeContent: () => List[ByteBuffer])(implicit ec: ExecutionContext): Unit = {
    filesToKeep ++= fileNames
    opFutures += prevFilesFuture.flatMap { prevFiles =>
      if (!changed && skipContentCheck && fileNames.forall(prevFiles.contains(_))) {
        Future.unit
      } else {
        ioThrottler.throttle {
          val fileContents = writeContent()
          assert(fileNames.size == fileContents.size)
          // Use a fold to sequence writes.
          fileNames.zip(fileContents).foldLeft(Future.unit) {
            case (prev, (fileName, fileContent)) =>
              prev.flatMap { _ =>
                outputImpl.writeFull(fileName, fileContent, skipContentCheck)
              }
          }
        }
      }
    }
  }

  def write(fileName: String, changed: Boolean)(
      writeContent: () => ByteBuffer)(implicit ec: ExecutionContext): Unit = {
    write(fileName :: Nil, changed)(() => writeContent() :: Nil)
  }

  def write(fileName: String)(writeContent: () => ByteBuffer)(
      implicit ec: ExecutionContext): Unit = {
    write(fileName, true)(writeContent)
  }

  def complete()(implicit ec: ExecutionContext): Future[_] = {
    opFutures += prevFilesFuture.flatMap { prevFiles =>
      val filesToRemove = prevFiles.diff(filesToKeep)
      Future.traverse(filesToRemove) { f =>
        ioThrottler.throttle(outputImpl.delete(f))
      }
    }
    Future.sequence[Any, mutable.ArrayBuffer](opFutures)
  }
}

private[backend] object OutputWriter {
  def start(output: OutputDirectory, maxConcurrentWrites: Int,
      skipContentCheck: Boolean)(implicit ec: ExecutionContext): OutputWriter = {
    val outputImpl = OutputDirectoryImpl.fromOutputDirectory(output)
    val prevFilesFuture = outputImpl.listFiles().map(_.toSet)
    new OutputWriter(outputImpl, prevFilesFuture, maxConcurrentWrites,
        skipContentCheck)
  }
}
