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
import scala.util.{Success, Failure}

import java.nio.ByteBuffer
import org.scalajs.linker.interface.OutputDirectory
import org.scalajs.linker.interface.unstable.OutputDirectoryImpl

/** Handles writing to an output directory.
 *
 *  Notably, handles removal of unnecessary files while being cache aware:
 *  Does *not* remove a file that needs no writing because it had a cache hit.
 */
private final class OutputWriter private (
    inputs: Iterator[OutputWriter.Input],
    outputImpl: OutputDirectoryImpl,
    prevFiles: Set[String],
    maxConcurrentWrites: Int,
    skipContentCheck: Boolean
)(implicit ec: ExecutionContext) {
  import OutputWriter._

  private val filesToRemove = mutable.Set.empty[String]
  filesToRemove ++= prevFiles

  private var usedSlots = 0
  private val completedPromise = Promise[Unit]()

  def run(): Future[Unit] = {
    startWork()
    completedPromise.future
  }

  private def startWork(): Unit = synchronized {
    // Kickoff as much processing as possible.
    while (usedSlots < maxConcurrentWrites && {
          usedSlots += 1
          work()
        }) {}
  }

  private def continueWork(): Unit = synchronized {
    val doMore = work()
    assert(!doMore || usedSlots == maxConcurrentWrites)
  }

  private def work(): Boolean = synchronized {
    if (inputs.hasNext) {
      val input = inputs.next()
      recordFilesToKeep(input) // under synchronization
      detach(writeInput(input))
      true // do more
    } else if (filesToRemove.size > 0) {
      // All input has been processed. Do file removal now.
      // Important: file removal tracking happens synchronously.
      // Otherwise this would be wrong!
      val fileToRemove = filesToRemove.head
      filesToRemove -= fileToRemove
      detach(outputImpl.delete(fileToRemove))
      true // do more
    } else {
      usedSlots -= 1 // release our slot
      if (usedSlots == 0)
        completedPromise.trySuccess(()) // everything is completed.
      false // stop
    }
  }

  private def detach(body: => Future[Unit]): Unit = synchronized {
    Future.unit.flatMap(_ => body).onComplete {
      case Failure(t)  => completedPromise.tryFailure(t)
      case Success(()) => continueWork() // re-use the same slot.
    }
  }

  private def recordFilesToKeep(input: Input) = synchronized {
    input match {
      case OneFile(fileName, _, _) =>
        filesToRemove -= fileName

      case TwoFiles(fileName1, fileName2, _, _) =>
        filesToRemove -= fileName1
        filesToRemove -= fileName2
    }
  }

  private def writeInput(input: Input): Future[Unit] = input match {
    case OneFile(fileName, changed, content) =>
      if (changed || !skipContentCheck || !prevFiles.contains(fileName))
        outputImpl.writeFull(fileName, content(), skipContentCheck)
      else
        Future.unit

    case TwoFiles(fileName1, fileName2, changed, content) =>
      if (changed || !skipContentCheck || !prevFiles.contains(fileName1) ||
          !prevFiles.contains(fileName2)) {
        val (c1, c2) = content()
        outputImpl.writeFull(fileName1, c1, skipContentCheck).flatMap { _ =>
          outputImpl.writeFull(fileName2, c2, skipContentCheck)
        }
      } else {
        Future.unit
      }
  }
}

private[backend] object OutputWriter {
  sealed trait Input

  case class OneFile(fileName: String, changed: Boolean, content: () => ByteBuffer) extends Input

  case class TwoFiles(fileName1: String, fileName2: String, changed: Boolean,
      content: () => (ByteBuffer, ByteBuffer))
      extends Input

  def write(inputs: Iterator[Input], output: OutputDirectory, maxConcurrentWrites: Int,
      skipContentCheck: Boolean)(implicit ec: ExecutionContext): Future[Unit] = {
    val outputImpl = OutputDirectoryImpl.fromOutputDirectory(output)
    outputImpl.listFiles().map(_.toSet).flatMap { prevFiles =>
      val outputWriter =
        new OutputWriter(inputs, outputImpl, prevFiles, maxConcurrentWrites, skipContentCheck)
      outputWriter.run()
    }
  }
}
