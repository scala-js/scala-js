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
    val builtinName: String, val availableInESVersion: ESVersion)

private[emitter] object PolyfillableBuiltin {
  lazy val All: List[PolyfillableBuiltin] = List(
    ObjectIsBuiltin,
    ImulBuiltin,
    FroundBuiltin,
    Clz32Builtin,
    PrivateSymbolBuiltin,
    GetOwnPropertyDescriptorsBuiltin
  )

  sealed abstract class GlobalVarBuiltin(val globalVar: String,
      builtinName: String, availableInESVersion: ESVersion)
      extends PolyfillableBuiltin(builtinName, availableInESVersion)

  sealed abstract class NamespacedBuiltin(val namespaceGlobalVar: String,
      builtinName: String, availableInESVersion: ESVersion)
      extends PolyfillableBuiltin(builtinName, availableInESVersion)

  case object ObjectIsBuiltin extends NamespacedBuiltin("Object", "is", ESVersion.ES2015)
  case object ImulBuiltin extends NamespacedBuiltin("Math", "imul", ESVersion.ES2015)
  case object FroundBuiltin extends NamespacedBuiltin("Math", "fround", ESVersion.ES2015)
  case object Clz32Builtin extends NamespacedBuiltin("Math", "clz32", ESVersion.ES2015)
  case object PrivateSymbolBuiltin
      extends GlobalVarBuiltin("Symbol", "privateJSFieldSymbol", ESVersion.ES2015)
  case object GetOwnPropertyDescriptorsBuiltin
      extends NamespacedBuiltin("Object", "getOwnPropertyDescriptors", ESVersion.ES2017)
}
