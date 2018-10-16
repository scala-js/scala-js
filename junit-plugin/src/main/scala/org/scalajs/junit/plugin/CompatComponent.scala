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

package org.scalajs.junit.plugin

import scala.reflect.internal.Flags
import scala.tools.nsc._

/** Hacks to have our source code compatible with all the versions of scalac
 *  that we support.
 *
 *  @author Nicolas Stucki
 */
trait CompatComponent {

  val global: Global

  import global._

  implicit final class DefinitionsCompat(
      self: CompatComponent.this.global.definitions.type) {

    def wrapVarargsArrayMethodName(elemtp: Type): TermName =
      self.wrapArrayMethodName(elemtp)

    def wrapArrayMethodName(elemtp: Type): TermName = infiniteLoop()
  }

  private def infiniteLoop(): Nothing =
    throw new AssertionError("Infinite loop in Compat210Component")
}
