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

import scala.concurrent._

import java.util.concurrent.atomic.AtomicBoolean

import org.scalajs.logging.Logger

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable._

/** Standard implementation of a Scala.js linker. */
private final class StandardLinkerImpl private (frontend: LinkerFrontend, backend: LinkerBackend)
    extends LinkerImpl {

  require(frontend.coreSpec == backend.coreSpec,
      "Frontend and backend must implement the same core specification")

  private[this] var _valid = true
  private[this] val _linking = new AtomicBoolean(false)

  def link(irFiles: Seq[IRFile], moduleInitializers: Seq[ModuleInitializer],
      output: OutputDirectory, logger: Logger)(implicit ec: ExecutionContext): Future[Report] = {
    if (!_linking.compareAndSet(false, true)) {
      throw new IllegalStateException("Linker used concurrently")
    }

    checkValid()
      .flatMap(_ =>
        frontend.link(irFiles ++ backend.injectedIRFiles, moduleInitializers,
            backend.symbolRequirements, logger)
      )
      .flatMap(linkingUnit => backend.emit(linkingUnit, output, logger))
      .andThen { case t if t.isFailure => _valid = false }
      .andThen { case t => _linking.set(false) }
  }

  private def checkValid(): Future[Unit] = {
    if (!_valid) {
      Future.failed(
          new IllegalStateException("Linker is invalid due to a previous exception in a component"))
    } else {
      Future.successful(())
    }
  }
}

object StandardLinkerImpl {
  def apply(frontend: LinkerFrontend, backend: LinkerBackend): Linker =
    new StandardLinkerImpl(frontend, backend)
}
