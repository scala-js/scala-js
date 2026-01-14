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
    ObjectIsBuiltin,
    ImulBuiltin,
    Clz32Builtin,
    FroundBuiltin,
    PrivateSymbolBuiltin,
    GetOwnPropertyDescriptorsBuiltin
  )

  sealed abstract class GlobalVarBuiltin(val globalVar: String,
      polyfillField: VarField, availableInESVersion: ESVersion)
      extends PolyfillableBuiltin(polyfillField, availableInESVersion)

  sealed abstract class NamespacedBuiltin(val namespaceGlobalVar: String,
      val builtinName: String, polyfillField: VarField, availableInESVersion: ESVersion)
      extends PolyfillableBuiltin(polyfillField, availableInESVersion)

  case object ObjectIsBuiltin
      extends NamespacedBuiltin("Object", "is", VarField.is, ESVersion.ES2015)

  case object ImulBuiltin extends NamespacedBuiltin("Math", "imul", VarField.imul, ESVersion.ES2015)

  case object Clz32Builtin
      extends NamespacedBuiltin("Math", "clz32", VarField.clz32, ESVersion.ES2015)

  case object FroundBuiltin
      extends NamespacedBuiltin("Math", "fround", VarField.fround, ESVersion.ES2015)

  case object PrivateSymbolBuiltin
      extends GlobalVarBuiltin("Symbol", VarField.privateJSFieldSymbol, ESVersion.ES2015)

  case object GetOwnPropertyDescriptorsBuiltin extends NamespacedBuiltin("Object",
          "getOwnPropertyDescriptors", VarField.getOwnPropertyDescriptors, ESVersion.ES2017)
}
