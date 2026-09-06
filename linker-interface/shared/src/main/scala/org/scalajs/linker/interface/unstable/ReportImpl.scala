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

package org.scalajs.linker.interface.unstable

import scala.collection.immutable

import org.scalajs.linker.interface._

final class ReportImpl(
    val publicModules: immutable.Iterable[Report.Module])
    extends Report

object ReportImpl {
  final class ModuleImpl(
      val moduleID: String,
      val moduleFileName: String,
      @deprecated(
          "sourceMapName is meaningless when targeting WebAssembly " +
          "(including with ModuleKind.ESModule); " +
          "the information can be found inside the JS or Wasm module files",
          since = "1.23.0")
      val sourceMapName: Option[String],
      val moduleKind: ModuleKind
  ) extends Report.Module
}
