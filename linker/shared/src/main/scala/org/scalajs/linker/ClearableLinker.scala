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

import scala.concurrent._

import org.scalajs.logging.Logger

import org.scalajs.linker.irio._

/** A box around a [[Linker]] to support clearing.
 *
 *  Calling `clear()` completely resets the state of this `ClearableLinker`, so
 *  that it can be used again without being affected by previous calls to
 *  `link`, even of those would have corrupted the internal state.
 *
 *  In addition to the contract of [[Linker]], if [[Linker.link]] throws an
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

object ClearableLinker {
  /** Creates a [[ClearableLinker]] from a function creating a [[Linker]].
   *
   *  Every time `clear()` is called, a new [[Linker]] is obtained from the
   *  `newLinker` function to ensure that all the previous state is discarded.
   *  `newLinker` must returned a new, independent instance of [[Linker]] every
   *  time it is called.
   *
   *  If `batchMode` is true, the returned `ClearableLinker` clears itself
   *  after every invocation of `link`.
   */
  def apply(newLinker: () => Linker, batchMode: Boolean): ClearableLinker =
    new ClearableLinkerImpl(newLinker, batchMode)

  private final class ClearableLinkerImpl(
      newLinker: () => Linker, batchMode: Boolean)
      extends ClearableLinker {

    private[this] var _linker: Linker = _

    def link(irFiles: Seq[VirtualScalaJSIRFile],
        moduleInitializers: Seq[ModuleInitializer],
        output: LinkerOutput, logger: Logger)(
        implicit ec: ExecutionContext): Future[Unit] = {
      linkerOp(_.link(irFiles, moduleInitializers, output, logger))
    }

    def clear(): Unit =
      _linker = null

    @inline
    private[this] def linkerOp[T](op: Linker => Future[T])(
        implicit ec: ExecutionContext): Future[T] = {
      ensureLinker()

      try {
        op(_linker).andThen {
          // Clear if we failed async or are in batch mode
          case t if t.isFailure || batchMode => clear()
        }
      } catch {
        // Clear if we throw
        case t: Throwable =>
          clear()
          throw t
      }
    }

    private def ensureLinker(): Unit = {
      // Ensure we have a linker
      if (_linker == null)
        _linker = newLinker()
    }
  }
}
