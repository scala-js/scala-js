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

/** A box around a [[Linker]] to support clearing.
 *
 *  Calling `clear()` completely resets the state of this `ClearableLinker`, so
 *  that it can be used again without being affected by previous calls to
 *  `link`, even of those would have corrupted the internal state.
 *
 *  In addition to the contract of [[Linker]], if {{Linker.link}} throws an
 *  exception, the `ClearableLinker` is automatically `clear()`'ed.
 *
 *  Implementations are allowed to automatically `clear()` in other cases, but
 *  never while a linking is in progress.
 *
 *  Unless otherwise specified, instances of this trait are not thread-safe.
 */
trait ClearableLinker extends Linker {
  /** Completely resets the state of this `ClearableLinker`.
   *
   *  After calling this method, this `ClearableLinker`, it can be used again
   *  without being affected by previous calls to `link`, even of those would
   *  have corrupted the internal state.
   */
  def clear(): Unit
}
