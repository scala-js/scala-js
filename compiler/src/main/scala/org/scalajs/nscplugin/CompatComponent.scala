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

package org.scalajs.nscplugin

import scala.collection.mutable

import scala.reflect.internal.Flags
import scala.tools.nsc._

/** Hacks to have our source code compatible with the compiler internals of all
 *  the versions of Scala that we support.
 *
 *  In general, it tries to provide the newer APIs on top of older APIs.
 *
 *  @author Sébastien Doeraene
 */
trait CompatComponent {
  import CompatComponent.infiniteLoop

  val global: Global

  import global._

  implicit final class SymbolCompat(self: Symbol) {
    def isScala3Defined: Boolean = false
  }

  implicit final class GlobalCompat(
      self: CompatComponent.this.global.type) {

    // Added in Scala 2.13.2 for configurable warnings
    object runReporting {
      def warning(pos: Position, msg: String, cat: Any, site: Symbol): Unit =
        reporter.warning(pos, msg)
    }
  }

  implicit final class TyperCompat(self: analyzer.Typer) {
    // Added in Scala 2.13.5 to make it clearer what is allowed since 2.13.4
    def checkClassOrModuleType(tpt: Tree): Boolean =
      self.checkClassType(tpt)

    def checkClassType(tpt: Tree): Boolean =
      infiniteLoop()
  }

  // DottyEnumSingleton was introduced in 2.13.6 to identify Scala 3 `enum` singleton cases.
  object AttachmentsCompatDef {
    object DottyEnumSingleton extends PlainAttachment
  }

  object AttachmentsCompat {
    import AttachmentsCompatDef._

    object Inner {
      import global._

      val DottyEnumSingletonAlias = DottyEnumSingleton
    }
  }

  lazy val DottyEnumSingletonCompat = AttachmentsCompat.Inner.DottyEnumSingletonAlias

  implicit final class SAMFunctionCompatOps(self: SAMFunction) {
    // Introduced in 2.12.5 to synthesize bridges in LMF classes
    def synthCls: Symbol = NoSymbol
  }

  /* global.genBCode.bTypes.initializeCoreBTypes()
   * Early 2.12.x versions require that this method be called from
   * GenJSCode.run(), but it disappeared later in the 2.12.x series.
   */

  implicit class BTypesCompat(bTypes: genBCode.bTypes.type) {
    def initializeCoreBTypes(): Unit = ()
  }

  // WarningCategory was added in Scala 2.13.2 for configurable warnings

  object WarningCategoryCompat {
    object Reporting {
      object WarningCategory {
        val Deprecation: Any = null
        val Other: Any = null
      }
    }
  }

  // Of type Reporting.WarningCategory.type, but we cannot explicit write it
  val WarningCategory = {
    import WarningCategoryCompat._

    {
      import scala.tools.nsc._
      Reporting.WarningCategory
    }
  }
}

object CompatComponent {
  private def infiniteLoop(): Nothing =
    throw new AssertionError("Infinite loop in Compat")
}
