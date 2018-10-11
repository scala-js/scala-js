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

package org.scalajs.core.tools.linker.backend

/** Kind of module structure emitted for the Scala.js output. */
sealed abstract class ModuleKind

object ModuleKind {

  /** All the available module kinds.
   *
   *  They are listed in decreasing order of "importance", as judged by
   *  whoever maintains the back-ends.
   */
  val All: List[ModuleKind] = List(
      NoModule,
      CommonJSModule)

  /** No module structure.
   *
   *  With this module kind, exports are stored on the global object by
   *  default, or to a separate object specified with
   *  `__ScalaJSEnv.exportsNamespace`.
   *
   *  Imports are not supported.
   */
  case object NoModule extends ModuleKind

  /** A CommonJS module (notably used by Node.js).
   *
   *  Imported modules are fetched with `require`. Exports go to the `exports`
   *  module-global variable.
   */
  case object CommonJSModule extends ModuleKind

}
