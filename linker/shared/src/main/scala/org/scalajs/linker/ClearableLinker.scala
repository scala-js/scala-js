/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.linker

import org.scalajs.logging.Logger
import org.scalajs.io._

import org.scalajs.linker.irio._

/** A box around a [[Linker]] to support clearing.
 *
 *  This further supports:
 *  - batch mode (clearing after every operation)
 *  - clearing if linker throws
 *
 *  This class is not thread-safe.
 */
final class ClearableLinker private (
    newLinker: () => Linker, batchMode: Boolean)
    extends Linker {

  private[this] var _linker: Linker = _

  def link(irFiles: Seq[VirtualScalaJSIRFile],
      moduleInitializers: Seq[ModuleInitializer],
      output: WritableVirtualJSFile, logger: Logger): Unit = {
    linkerOp(_.link(irFiles, moduleInitializers, output, logger))
  }

  def clear(): Unit =
    _linker = null

  @inline
  private[this] def linkerOp[T](op: Linker => T): T = {
    ensureLinker()

    try {
      op(_linker)
    } catch {
      // Clear if we throw
      case t: Throwable =>
        clear()
        throw t
    } finally {
      // Clear if we are in batch mode
      if (batchMode)
        clear()
    }
  }

  private def ensureLinker(): Unit = {
    // Ensure we have a linker
    if (_linker == null)
      _linker = newLinker()
  }
}

object ClearableLinker {
  def apply(newLinker: () => Linker, batchMode: Boolean): ClearableLinker =
    new ClearableLinker(newLinker, batchMode)
}
