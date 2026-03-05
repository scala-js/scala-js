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

/** Kind of module structure emitted for the Scala.js output. */
abstract class ModuleKind private ()

object ModuleKind {

  /** All the available module kinds.
   *
   *  They are listed in decreasing order of "importance", as judged by
   *  whoever maintains the back-ends.
   */
  val All: List[ModuleKind] = List(
      NoModule,
      ESModule,
      CommonJSModule)

  /** No module structure.
   *
   *  With this module kind, exports are stored on the global object.
   *
   *  Imports are not supported.
   */
  case object NoModule extends ModuleKind

  /** An ECMAScript 2015 module.
   *
   *  Scala.js imports and exports directly map to `import` and `export`
   *  clauses in the ES module.
   */
  case object ESModule extends ModuleKind

  /** A CommonJS module (notably used by Node.js).
   *
   *  Imported modules are fetched with `require`. Exports go to the `exports`
   *  module-global variable.
   */
  case object CommonJSModule extends ModuleKind

  final val NoModuleStr = "NoModule"
  final val ESModuleStr = "ESModule"
  final val CommonJSModuleStr = "CommonJSModule"

  def serialize(moduleKind: ModuleKind): String = {
    moduleKind match {
      case NoModule       => NoModuleStr
      case ESModule       => ESModuleStr
      case CommonJSModule => CommonJSModuleStr
    }
  }

  def deserialize(value: String): Option[ModuleKind] = {
    value.trim match {
      case NoModuleStr | "none"           => Some(NoModule)
      case ESModuleStr | "es"             => Some(ESModule)
      case CommonJSModuleStr | "commonjs" => Some(CommonJSModule)
      case _                              => None
    }
  }

  private[interface] implicit object ModuleKindFingerprint extends Fingerprint[ModuleKind] {

    override def fingerprint(moduleKind: ModuleKind): String = {
      moduleKind match {
        case NoModule       => NoModuleStr
        case ESModule       => ESModuleStr
        case CommonJSModule => CommonJSModuleStr
      }
    }
  }
}
