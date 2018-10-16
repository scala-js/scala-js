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

import scala.reflect.internal.Flags.{LOCAL, PRIVATE}
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

  implicit final class GlobalCompat(
      self: CompatComponent.this.global.type) {

    object originalOwner {
      def getOrElse(sym: Symbol, orElse: => Symbol): Symbol = infiniteLoop()
    }
  }

  lazy val scalaUsesImplClasses: Boolean =
    definitions.SeqClass.implClass != NoSymbol // a trait we know has an impl class

  implicit final class StdTermNamesCompat(self: global.nme.type) {
    def IMPL_CLASS_SUFFIX: String = noImplClasses()

    def isImplClassName(name: Name): Boolean = false
  }

  implicit final class StdTypeNamesCompat(self: global.tpnme.type) {
    def IMPL_CLASS_SUFFIX: String = noImplClasses()

    def interfaceName(implname: Name): TypeName = noImplClasses()
  }

  // SAMFunction was introduced in 2.12.0-M4 for LMF-capable SAM types

  object SAMFunctionAttachCompatDef {
    case class SAMFunction(samTp: Type, sam: Symbol, synthCls: Symbol)
        extends PlainAttachment
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
   *
   * This one has a very particular history:
   * - in 2.11.{0-1}, genBCode does not have a bTypes member
   * - In 2.11.{2-5}, there is genBCode.bTypes, but it has no
   *   initializeCoreBTypes (it was actually typo'ed as intializeCoreBTypes!)
   * - In 2.11.6+, including 2.12, we finally have
   *   genBCode.bTypes.initializeCoreBTypes
   * - Since 2.12.0-M4, it is mandatory to call that method from GenJSCode.run()
   */

  object LowPrioGenBCodeCompat {
    object genBCode {
      object bTypes {
        def initializeCoreBTypes(): Unit = ()
      }
    }
  }

  def initializeCoreBTypesCompat(): Unit = {
    import LowPrioGenBCodeCompat.genBCode._

    {
      import genBCode._

      import LowPrioGenBCodeCompat.genBCode.bTypes._

      {
        import bTypes._

        initializeCoreBTypes()
      }
    }
  }
}

object CompatComponent {
  private def infiniteLoop(): Nothing =
    throw new AssertionError("Infinite loop in Compat")

  private def noImplClasses(): Nothing =
    throw new AssertionError("No impl classes in this version")
}
