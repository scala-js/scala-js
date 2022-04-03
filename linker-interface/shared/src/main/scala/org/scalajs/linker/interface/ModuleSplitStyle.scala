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

import java.nio.CharBuffer
import java.nio.charset.{StandardCharsets, CharacterCodingException}

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

  /** Mix between [[FewestModules]] / [[SmallestModules]]
   *
   *  * Make modules as small as possible for all classes in any of [[packages]]
   *    (or subpackages thereof).
   *  * Make as few modules as possible for everything else (while not including
   *    unnecessary code).
   *
   *  @note In order to avoid ambiguity between packages and classes (and keep a
   *     simple interface), selecting individual classes is not supported.
   */
  final case class SmallModulesFor(packages: List[String]) extends ModuleSplitStyle {
    require(packages.nonEmpty, "must have at least one package")
    packages.foreach(p => require(isValidPackage(p), s"invalid package name $p"))
  }

  private[interface] implicit object ModuleSplitStyleFingerprint
      extends Fingerprint[ModuleSplitStyle] {

    override def fingerprint(moduleSplitStyle: ModuleSplitStyle): String = {
      moduleSplitStyle match {
        case FewestModules             => "FewestModules"
        case SmallestModules           => "SmallestModules"
        case SmallModulesFor(packages) => s"SmallModulesFor($packages)"
      }
    }
  }

  private def isValidPackage(pkg: String): Boolean = {
    pkg.nonEmpty && isValidUTF16(pkg) &&
    List(':', '[', '/').forall(!pkg.contains(_)) &&
    pkg.split("\\.", -1).forall(_.nonEmpty)
  }

  private def isValidUTF16(str: String): Boolean = {
    try {
      StandardCharsets.UTF_16.newEncoder().encode(CharBuffer.wrap(str))
      true
    } catch {
      case _: CharacterCodingException => false
    }
  }
}
