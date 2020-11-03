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
  import CompatComponent.{infiniteLoop, noImplClasses}

  val global: Global

  import global._

  implicit final class SymbolCompat(self: Symbol) {
    def originalOwner: Symbol =
      global.originalOwner.getOrElse(self, self.rawowner)

    def implClass: Symbol = NoSymbol

    def isTraitOrInterface: Boolean = self.isTrait || self.isInterface
  }

  implicit final class GlobalCompat(self: CompatComponent.this.global.type) {

    object originalOwner {
      def getOrElse(sym: Symbol, orElse: => Symbol): Symbol = infiniteLoop()
    }

    // Added in Scala 2.13.2 for configurable warnings
    object runReporting {
      def warning(pos: Position, msg: String, cat: Any, site: Symbol): Unit =
        reporter.warning(pos, msg)
    }
  }

  private implicit final class FlagsCompat(self: Flags.type) {
    def IMPLCLASS: Long = infiniteLoop()
  }

  lazy val scalaUsesImplClasses: Boolean =
    definitions.SeqClass.implClass != NoSymbol // a trait we know has an impl class

  def isImplClass(sym: Symbol): Boolean =
    scalaUsesImplClasses && sym.hasFlag(Flags.IMPLCLASS)

  implicit final class StdTermNamesCompat(self: global.nme.type) {
    def IMPL_CLASS_SUFFIX: String = noImplClasses()

    def isImplClassName(name: Name): Boolean = false
  }

  implicit final class StdTypeNamesCompat(self: global.tpnme.type) {
    def IMPL_CLASS_SUFFIX: String = noImplClasses()

    def interfaceName(implname: Name): TypeName = noImplClasses()
  }

  // SAMFunction was introduced in 2.12 for LMF-capable SAM types

  object SAMFunctionAttachCompatDef {
    case class SAMFunction(samTp: Type, sam: Symbol, synthCls: Symbol) extends PlainAttachment
  }

  object SAMFunctionAttachCompat {
    import SAMFunctionAttachCompatDef._

    object Inner {
      import global._

      type SAMFunctionAlias = SAMFunction
      val SAMFunctionAlias = SAMFunction
    }
  }

  type SAMFunctionCompat = SAMFunctionAttachCompat.Inner.SAMFunctionAlias
  lazy val SAMFunctionCompat = SAMFunctionAttachCompat.Inner.SAMFunctionAlias

  implicit final class SAMFunctionCompatOps(self: SAMFunctionCompat) {
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

  private def noImplClasses(): Nothing =
    throw new AssertionError("No impl classes in this version")
}
