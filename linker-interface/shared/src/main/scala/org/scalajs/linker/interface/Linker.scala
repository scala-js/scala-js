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
  final def link(irFiles: Seq[IRFile],
      moduleInitializers: Seq[ModuleInitializer],
      output: LinkerOutput, logger: Logger)(
      implicit ec: ExecutionContext): Future[Unit] = {
    val module = ModuleOutputSpec.Module("core")
      .withInitializers(moduleInitializers: _*)

    val spec = ModuleOutputSpec()
      .addModule(ModuleOutputSpec.Selector.default, module)

    val moduleOutput = ModuleLinkerOutput(module.id, output)

    link(irFiles, spec, Seq(moduleOutput), logger)
  }

  def link(irFiles: Seq[IRFile], moduleSpec: ModuleOutputSpec, outputs: Seq[ModuleLinkerOutput], logger: Logger)(
      implicit ec: ExecutionContext): Future[Unit]
}
