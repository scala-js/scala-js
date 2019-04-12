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
  /** All files are to be loaded as scripts into the global scope in the order given. */
  final case class ScriptsToLoad(scripts: List[Path]) extends Input

  /** All files are to be loaded as ES modules, in the given order.
   *
   *  Some environments may not be able to execute several ES modules in a
   *  deterministic order. If that is the case, they must reject an
   *  `ESModulesToLoad` input if the `modules` argument has more than one
   *  element.
   */
  final case class ESModulesToLoad(modules: List[Path])
      extends Input

  /** All files are to be loaded as CommonJS modules, in the given order. */
  final case class CommonJSModulesToLoad(modules: List[Path])
      extends Input
}

class UnsupportedInputException(msg: String, cause: Throwable)
    extends IllegalArgumentException(msg, cause) {
  def this(msg: String) = this(msg, null)
  def this(input: Input) = this(s"Unsupported input: $input")
}
