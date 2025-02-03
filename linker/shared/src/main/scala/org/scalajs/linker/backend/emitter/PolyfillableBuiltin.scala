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

package org.scalajs.linker.backend.emitter

import org.scalajs.linker.interface.ESVersion

private[emitter] sealed abstract class PolyfillableBuiltin(
    val polyfillField: VarField, val availableInESVersion: ESVersion)

private[emitter] object PolyfillableBuiltin {
  lazy val All: List[PolyfillableBuiltin] = List(
    GetOwnPropertyDescriptorsBuiltin
  )

  sealed abstract class GlobalVarBuiltin(val globalVar: String,
      polyfillField: VarField, availableInESVersion: ESVersion)
      extends PolyfillableBuiltin(polyfillField, availableInESVersion)

  sealed abstract class NamespacedBuiltin(val namespaceGlobalVar: String,
      val builtinName: String, polyfillField: VarField, availableInESVersion: ESVersion)
      extends PolyfillableBuiltin(polyfillField, availableInESVersion)

  case object GetOwnPropertyDescriptorsBuiltin extends NamespacedBuiltin("Object",
      "getOwnPropertyDescriptors", VarField.getOwnPropertyDescriptors, ESVersion.ES2017)
}
