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

package org.scalajs.jsenv

import java.nio.file.Path

/** Input to a [[JSEnv]].
 *
 *  Implementors of a [[JSEnv]] are expected to pattern match on this input
 *  type and handle the ones they support.
 *
 *  Note that this type is not sealed, so future versions of Scala.js may add
 *  additional input types. Older [[JSEnv]]s are expected to fail in this case
 *  with an [[UnsupportedInputException]].
 */
abstract class Input private ()

object Input {
  /** The file is to be loaded as a script into the global scope. */
  final case class Script(script: Path) extends Input

  /** The file is to be loaded as an ES module.
   *
   *  Some environments may not be able to load several ES modules in a
   *  deterministic order. If that is the case, they must reject an
   *  `ESModule` input if it appears with other Inputs such that loading
   *  in a deterministic order is not possible.
   */
  final case class ESModule(module: Path) extends Input

  /** The file is to be loaded as a CommonJS module. */
  final case class CommonJSModule(module: Path) extends Input
}

class UnsupportedInputException(msg: String, cause: Throwable)
    extends IllegalArgumentException(msg, cause) {
  def this(msg: String) = this(msg, null)
  def this(input: Seq[Input]) = this(s"Unsupported input: $input")
}
