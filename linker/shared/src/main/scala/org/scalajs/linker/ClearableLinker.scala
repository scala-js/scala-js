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

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable.LinkerImpl
import org.scalajs.logging.Logger

object ClearableLinker {
  /** Creates a [[interface.ClearableLinker]] from a function creating a [[interface.Linker]].
   *
   *  Every time `clear()` is called, a new [[interface.Linker]] is obtained from
   *  the `newLinker` function to ensure that all the previous state is discarded.
   *  `newLinker` must returned a new, independent instance of [[interface.Linker]]
   *  every time it is called.
   *
   *  If `batchMode` is true, the returned `ClearableLinker` clears itself
   *  after every invocation of `link`.
   */
  def apply(newLinker: () => Linker, batchMode: Boolean): ClearableLinker =
    new ClearableLinkerImpl(newLinker, batchMode)

  private final class ClearableLinkerImpl(
      newLinker: () => Linker, batchMode: Boolean)
      extends LinkerImpl with ClearableLinker {

    private[this] var _linker: Linker = _

    def link(irFiles: Seq[IRFile],
        moduleInitializers: Seq[ModuleInitializer],
        output: OutputDirectory, logger: Logger)(
        implicit ec: ExecutionContext): Future[Report] = {
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
