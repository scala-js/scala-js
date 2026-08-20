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

package scala.scalajs

object LinkingInfo {
  def moduleKind: Int = ModuleKind.NoModule

  def linkTimeIf[T](cond: Boolean)(thenp: T)(elsep: T): T =
    if (cond) thenp else elsep

  object ModuleKind {
    final val NoModule = 1
    final val MinimalWasmModule = 4
  }
}
