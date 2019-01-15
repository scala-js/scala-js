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

package org.scalajs.linker.standard

import scala.language.implicitConversions

import java.util.concurrent.atomic.AtomicBoolean

import org.scalajs.logging.Logger
import org.scalajs.io._

import org.scalajs.linker._
import org.scalajs.linker.irio._

/** Standard implementation of a Scala.js linker. */
private final class StandardLinkerImpl private (
    frontend: LinkerFrontend, backend: LinkerBackend)
    extends Linker {

  require(frontend.coreSpec == backend.coreSpec,
      "Frontend and backend must implement the same core specification")

  private[this] var _valid = true
  private[this] val _linking = new AtomicBoolean(false)

  def link(irFiles: Seq[VirtualScalaJSIRFile],
      moduleInitializers: Seq[ModuleInitializer],
      output: LinkerOutput, logger: Logger): Unit = {
    guard {
      val unit = frontend.link(irFiles ++ backend.injectedIRFiles,
          moduleInitializers, backend.symbolRequirements, logger)
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

object StandardLinkerImpl {
  def apply(frontend: LinkerFrontend, backend: LinkerBackend): Linker =
    new StandardLinkerImpl(frontend, backend)
}
