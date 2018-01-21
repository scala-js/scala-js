/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.linker

import scala.language.implicitConversions

import java.util.concurrent.atomic.AtomicBoolean

import org.scalajs.logging.Logger
import org.scalajs.io._

import org.scalajs.linker.analyzer.SymbolRequirement
import org.scalajs.linker.frontend.LinkerFrontend
import org.scalajs.linker.frontend.optimizer.IncOptimizer
import org.scalajs.linker.backend.{LinkerBackend, BasicLinkerBackend}
import org.scalajs.linker.irio._

/** The Scala.js linker */
final class Linker private (frontend: LinkerFrontend, backend: LinkerBackend)
    extends GenLinker {

  require(frontend.coreSpec == backend.coreSpec,
      "Frontend and backend must implement the same core specification")

  private[this] var _valid = true
  private[this] val _linking = new AtomicBoolean(false)

  def link(irFiles: Seq[VirtualScalaJSIRFile],
      moduleInitializers: Seq[ModuleInitializer],
      output: WritableVirtualJSFile, logger: Logger): Unit = {
    guard {
      val unit = frontend.link(irFiles, moduleInitializers,
          backend.symbolRequirements, logger)
      backend.emit(unit, output, logger)
    }
  }

  @inline
  private[this] def guard[T](body: => T): T = {
    if (!_linking.compareAndSet(false, true)) {
      throw new IllegalStateException("Linker used concurrently")
    }

    try {
      if (!_valid) {
        throw new IllegalStateException(
          "Linker is invalid due to a previous exception in a component")
      }

      body
    } catch {
      case t: Throwable =>
        _valid = false
        throw t
    } finally {
      _linking.set(false)
    }
  }
}

object Linker {
  def apply(frontend: LinkerFrontend, backend: LinkerBackend): Linker =
    new Linker(frontend, backend)
}
