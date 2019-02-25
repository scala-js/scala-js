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

/** A Scala.js linker, with its most abstract API.
 *
 *  A linker can take a sequence of virtual .sjsir files and a sequence of
 *  module initializers, link them together, and write the output to a writable
 *  .js file.
 */
abstract class Linker private[linker] () {
  def link(irFiles: Seq[VirtualScalaJSIRFile],
      moduleInitializers: Seq[ModuleInitializer],
      output: LinkerOutput, logger: Logger)(
      implicit ec: ExecutionContext): Future[Unit]
}
