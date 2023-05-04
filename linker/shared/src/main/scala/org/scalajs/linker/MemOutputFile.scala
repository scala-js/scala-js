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

package org.scalajs.linker

import org.scalajs.linker.interface.LinkerOutput
import org.scalajs.linker.interface.unstable.OutputFileImpl

@deprecated("Part of old Linker interface. Use MemOutputDirectory instead.", "1.3.0")
sealed trait MemOutputFile extends LinkerOutput.File {
  /** Content that has been written to this [[MemOutputFile]].
   *
   *  @throws java.lang.IllegalStateException if nothing has been written yet.
   */
  def content: Array[Byte]
}

@deprecated("Part of old Linker interface. Use MemOutputDirectory instead.", "1.3.0")
object MemOutputFile {
  private final val MemFileName = "mem-file.js"

  def apply(): MemOutputFile = new Impl(MemOutputDirectory())

  private final class Impl(dir: MemOutputDirectory)
      extends OutputFileImpl(MemFileName, dir) with MemOutputFile {
    def content: Array[Byte] = {
      dir.content(name).getOrElse {
        throw new IllegalStateException("content hasn't been written yet")
      }
    }
  }
}
