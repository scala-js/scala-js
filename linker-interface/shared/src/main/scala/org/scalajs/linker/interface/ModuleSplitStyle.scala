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

/** How to split the output into modules. */
abstract class ModuleSplitStyle private ()

object ModuleSplitStyle {

  /** Make as few modules as possible (while not including unnecessary code).
   *
   *  This is the default and the only style that works with
   *  [[ModuleKind.NoModule]].
   */
  case object FewestModules extends ModuleSplitStyle

  /** Make modules as small as possible. */
  case object SmallestModules extends ModuleSplitStyle

  private[interface] implicit object ModuleSplitStyleFingerprint
      extends Fingerprint[ModuleSplitStyle] {

    override def fingerprint(moduleSplitStyle: ModuleSplitStyle): String = {
      moduleSplitStyle match {
        case FewestModules   => "FewestModules"
        case SmallestModules => "SmallestModules"
      }
    }
  }
}
