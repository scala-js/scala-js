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

abstract class Report private[interface] {
  def publicModules: Iterable[Report.Module]
}

object Report {
  abstract class Module private[interface] {
    def moduleID: String
    def jsFileName: String
    def sourceMapName: Option[String]
    def moduleKind: ModuleKind
  }
}
