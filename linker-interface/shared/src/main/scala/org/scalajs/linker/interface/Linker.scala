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

import scala.concurrent._

import org.scalajs.logging.Logger

/** A Scala.js linker, with its most abstract API.
 *
 *  A linker can take a sequence of virtual .sjsir files and a sequence of
 *  module initializers, link them together, and write the output to a writable
 *  .js file.
 */
abstract class Linker private[interface] () {
  def link(irFiles: Seq[IRFile],
      moduleInitializers: Seq[ModuleInitializer],
      output: OutputDirectory, logger: Logger)(
      implicit ec: ExecutionContext): Future[Report]

  final def link(irFiles: Seq[IRFile],
      moduleInitializers: Seq[ModuleInitializer],
      output: LinkerOutput, logger: Logger)(
      implicit ec: ExecutionContext): Future[Unit] = {
    // TODO:
    // 1. Run the linker, with output in memory.
    // 2. Recover the JS and source map file (using the LinkingReport).
    // 3. Copy the output to the actual files, replacing the sourcemap / js file links.

    ???
  }
}
