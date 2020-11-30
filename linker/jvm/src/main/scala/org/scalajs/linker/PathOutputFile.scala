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

import java.nio.file.Path

import org.scalajs.linker.interface.LinkerOutput
import org.scalajs.linker.interface.unstable.OutputFileImpl

@deprecated("Part of old Linker interface. Use PathOutputDirectory instead.", "1.3.0")
object PathOutputFile {
  def apply(path: Path): LinkerOutput.File = {
    val np = path.toAbsolutePath().normalize()
    val dir = PathOutputDirectory(np.getParent())
    new OutputFileImpl(np.getFileName().toString(), dir)
  }

  def atomic(path: Path): LinkerOutput.File =
    apply(path) // PathOutputDirectory is always atomic.
}
