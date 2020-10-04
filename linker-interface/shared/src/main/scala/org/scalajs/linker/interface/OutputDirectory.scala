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

package org.scalajs.linker.interface

import org.scalajs.linker.interface.unstable.OutputDirectoryImpl

/** Directory where the linker will write its output files. */
abstract class OutputDirectory private[interface] () {
  private[interface] def impl: OutputDirectoryImpl
}
