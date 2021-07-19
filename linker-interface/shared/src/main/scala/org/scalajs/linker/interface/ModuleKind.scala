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
      WeakNoModule,
      ESModule,
      CommonJSModule)

  /** No module structure.
   *
   *  With this module kind, exports are stored on the global object.
   *
   *  Imports are not supported.
   */
  case object NoModule extends ModuleKind

  /** Weak No module structure.
   *
   *  With this module kind, exports are stored on the global object.
   *
   *  Imports are not supported.
   *
   *  Analyzer ignores all modules warnings.
   */
  case object WeakNoModule extends ModuleKind

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

  private[interface] implicit object ModuleKindFingerprint
      extends Fingerprint[ModuleKind] {

    override def fingerprint(moduleKind: ModuleKind): String = {
      moduleKind match {
        case NoModule       => "NoModule"
        case WeakNoModule   => "WeakNoModule"
        case ESModule       => "ESModule"
        case CommonJSModule => "CommonJSModule"
      }
    }
  }
}
