/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.linker

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
      CommonJSModule)

  /** No module structure.
   *
   *  With this module kind, exports are stored on the global object.
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
