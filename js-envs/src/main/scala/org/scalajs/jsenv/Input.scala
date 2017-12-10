/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js JS Envs           **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2017, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv

import org.scalajs.io._

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
  final case class ScriptsToLoad(scripts: List[VirtualJSFile]) extends Input
}

case class UnsupportedInputException(msg: String, cause: Throwable)
    extends IllegalArgumentException(msg, cause) {
  def this(msg: String) = this(msg, null)
  def this(input: Input) = this(s"Unsupported input: $input")
}
