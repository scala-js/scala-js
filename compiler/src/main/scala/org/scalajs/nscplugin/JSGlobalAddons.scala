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

import scala.tools.nsc._

import scala.collection.mutable

import org.scalajs.ir.Trees.JSNativeLoadSpec
import org.scalajs.ir.{Trees => js}

/** Additions to Global meaningful for the JavaScript backend
 *
 *  @author Sébastien Doeraene
 */
trait JSGlobalAddons extends JSDefinitions
                        with CompatComponent {
  val global: Global

  import global._
  import jsDefinitions._
  import definitions._

  /** JavaScript primitives, used in jscode */
  object jsPrimitives extends JSPrimitives {
    val global: JSGlobalAddons.this.global.type = JSGlobalAddons.this.global
    val jsAddons: ThisJSGlobalAddons =
      JSGlobalAddons.this.asInstanceOf[ThisJSGlobalAddons]
  }

  /** Extracts the super type of a `Template`, with type parameters reinvented
   *  so that the type is well-formed outside of the `Template`, i.e., at the
   *  same level where the corresponding `ImplDef` is defined.
   */
  def extractSuperTpeFromImpl(impl: Template): Type =
    reinventTypeParams(impl.parents.head.tpe)

  /** Reinvents all the type parameters of a `TypeRef`.
   *
   *  This is done by existentially quantifying over all type parameters of
   *  the class type referenced by the `TypeRef`.
   *
   *  As a simple example, given the definition
   *  {{{
   *  class C[A, B <: AnyVal]
   *  }}}
   *  this transforms
   *  {{{
   *  path.C[A, Int]
   *  }}}
   *  into
   *  {{{
   *  path.C[_, _ <: AnyVal]
   *  }}}
   *
   *  As a complex example, given the definition
   *  {{{
   *  class D[A, B <: List[Seq[A]]]
   *  }}}
   *  this method transforms
   *  {{{
   *  path.D[?0, ?1] forSome { type ?0; type ?1 <: List[Seq[?0]] }
   *  }}}
   */
  private def reinventTypeParams(tp: Type): Type = {
    tp match {
      case TypeRef(pre, sym, _) if sym.isClass && sym.typeParams.nonEmpty =>
        val eparams = typeParamsToExistentials(sym)
        existentialAbstraction(eparams, typeRef(pre, sym, eparams.map(_.tpe)))
      case _ =>
        tp
    }
  }

  /** global javascript interop related helpers */
  object jsInterop {
    import scala.reflect.NameTransformer
    import scala.reflect.internal.Flags

    /** TopLevel exports, by owner. */
    private val topLevelExports =
      mutable.Map.empty[Symbol, List[TopLevelExportInfo]]

    /** Static exports, by owner. */
    private val staticExports =
      mutable.Map.empty[Symbol, List[StaticExportInfo]]

    /** JS native load specs of the symbols in the current compilation run. */
    private val jsNativeLoadSpecs =
      mutable.Map.empty[Symbol, JSNativeLoadSpec]

    private val exportPrefix = "$js$exported$"
    private val methodExportPrefix = exportPrefix + "meth$"
    private val propExportPrefix = exportPrefix + "prop$"

    /** Info for a non-member export. */
    sealed trait ExportInfo {
      val pos: Position
    }

    /* Not final because it causes the follwing compile warning:
     * "The outer reference in this type test cannot be checked at run time."
     */
    case class TopLevelExportInfo(moduleID: String, jsName: String)(
        val pos: Position) extends ExportInfo
    case class StaticExportInfo(jsName: String)(val pos: Position)
        extends ExportInfo

    sealed abstract class JSName {
      def displayName: String
    }

    object JSName {
      // Not final because it causes annoying compile warnings
      case class Literal(name: String) extends JSName {
        def displayName: String = name
      }

      // Not final because it causes annoying compile warnings
      case class Computed(sym: Symbol) extends JSName {
        def displayName: String = sym.fullName
      }
    }

    sealed abstract class JSCallingConvention {
      def displayName: String
    }

    object JSCallingConvention {
      case object Call extends JSCallingConvention {
        def displayName: String = "function application"
      }

      case object BracketAccess extends JSCallingConvention {
        def displayName: String = "bracket access"
      }

      case object BracketCall extends JSCallingConvention {
        def displayName: String = "bracket call"
      }

      case class Method(name: JSName) extends JSCallingConvention {
        def displayName: String = "method '" + name.displayName + "'"
      }

      case class Property(name: JSName) extends JSCallingConvention {
        def displayName: String = "property '" + name.displayName + "'"
      }

      case class UnaryOp(code: js.JSUnaryOp.Code) extends JSCallingConvention {
        def displayName: String = "unary operator"
      }

      case class BinaryOp(code: js.JSBinaryOp.Code) extends JSCallingConvention {
        def displayName: String = "binary operator"
      }

      def of(sym: Symbol): JSCallingConvention = {
        assert(sym.isTerm, s"got non-term symbol: $sym")

        if (isJSBracketAccess(sym)) {
          BracketAccess
        } else if (isJSBracketCall(sym)) {
          BracketCall
        } else {
          def default = {
            val jsName = jsNameOf(sym)
            if (isJSProperty(sym)) Property(jsName)
            else Method(jsName)
          }

          if (!sym.hasAnnotation(JSNameAnnotation)) {
            lazy val pc = sym.paramss.map(_.size).sum

            sym.name match {
              case nme.apply => Call

              case JSUnaryOpMethodName(code) if pc == 0 =>
                UnaryOp(code)

              case JSBinaryOpMethodName(code) if pc == 1 =>
                BinaryOp(code)

              case _ =>
                default
            }
          } else {
            default
          }
        }
      }
    }

    private object JSUnaryOpMethodName {
      private val map = Map[Name, js.JSUnaryOp.Code](
        nme.UNARY_+ -> js.JSUnaryOp.+,
        nme.UNARY_- -> js.JSUnaryOp.-,
        nme.UNARY_~ -> js.JSUnaryOp.~,
        nme.UNARY_! -> js.JSUnaryOp.!
      )

      /* We use Name instead of TermName to work around
       * https://github.com/scala/bug/issues/11534
       */
      def unapply(name: Name): Option[js.JSUnaryOp.Code] =
        map.get(name)
    }

    private object JSBinaryOpMethodName {
      private val map = Map[Name, js.JSBinaryOp.Code](
        nme.ADD -> js.JSBinaryOp.+,
        nme.SUB -> js.JSBinaryOp.-,
        nme.MUL -> js.JSBinaryOp.*,
        nme.DIV -> js.JSBinaryOp./,
        nme.MOD -> js.JSBinaryOp.%,

        nme.LSL -> js.JSBinaryOp.<<,
        nme.ASR -> js.JSBinaryOp.>>,
        nme.LSR -> js.JSBinaryOp.>>>,
        nme.OR  -> js.JSBinaryOp.|,
        nme.AND -> js.JSBinaryOp.&,
        nme.XOR -> js.JSBinaryOp.^,

        nme.LT -> js.JSBinaryOp.<,
        nme.LE -> js.JSBinaryOp.<=,
        nme.GT -> js.JSBinaryOp.>,
        nme.GE -> js.JSBinaryOp.>=,

        nme.ZAND -> js.JSBinaryOp.&&,
        nme.ZOR  -> js.JSBinaryOp.||
      )

      /* We use Name instead of TermName to work around
       * https://github.com/scala/bug/issues/11534
       */
      def unapply(name: Name): Option[js.JSBinaryOp.Code] =
        map.get(name)
    }

    def clearGlobalState(): Unit = {
      topLevelExports.clear()
      staticExports.clear()
      jsNativeLoadSpecs.clear()
    }

    def registerTopLevelExports(sym: Symbol, infos: List[TopLevelExportInfo]): Unit = {
      assert(!topLevelExports.contains(sym), s"symbol exported twice: $sym")
      topLevelExports.put(sym, infos)
    }

    def registerStaticExports(sym: Symbol, infos: List[StaticExportInfo]): Unit = {
      assert(!staticExports.contains(sym), s"symbol exported twice: $sym")
      staticExports.put(sym, infos)
    }

    def topLevelExportsOf(sym: Symbol): List[TopLevelExportInfo] =
      topLevelExports.getOrElse(sym, Nil)

    def staticExportsOf(sym: Symbol): List[StaticExportInfo] =
      staticExports.getOrElse(sym, Nil)

    /** creates a name for an export specification */
    def scalaExportName(jsName: String, isProp: Boolean): TermName = {
      val pref = if (isProp) propExportPrefix else methodExportPrefix
      val encname = NameTransformer.encode(jsName)
      newTermName(pref + encname)
    }

    /** checks if the given symbol is a JSExport */
    def isExport(sym: Symbol): Boolean =
      sym.name.startsWith(exportPrefix) && !sym.hasFlag(Flags.DEFAULTPARAM)

    /** retrieves the originally assigned jsName of this export and whether it
     *  is a property
     */
    def jsExportInfo(name: Name): (String, Boolean) = {
      def dropPrefix(prefix: String) ={
        if (name.startsWith(prefix)) {
          // We can't decode right away due to $ separators
          val enc = name.toString.substring(prefix.length)
          Some(NameTransformer.decode(enc))
        } else None
      }

      dropPrefix(methodExportPrefix).map((_,false)).orElse {
        dropPrefix(propExportPrefix).map((_,true))
      }.getOrElse {
        throw new IllegalArgumentException(
            "non-exported name passed to jsExportInfo")
      }
    }

    def jsclassAccessorFor(clazz: Symbol): Symbol =
      clazz.owner.info.member(clazz.name.append("$jsclass").toTermName)

    def isJSProperty(sym: Symbol): Boolean = isJSGetter(sym) || isJSSetter(sym)

    @inline private def enteringUncurryIfAtPhaseAfter[A](op: => A): A = {
      if (currentRun.uncurryPhase != NoPhase &&
          isAtPhaseAfter(currentRun.uncurryPhase)) {
        enteringPhase(currentRun.uncurryPhase)(op)
      } else {
        op
      }
    }

    /** has this symbol to be translated into a JS getter (both directions)? */
    def isJSGetter(sym: Symbol): Boolean = {
      /* `sym.isModule` implies that `sym` is the module's accessor. In 2.12,
       * module accessors are synthesized
       * after uncurry, thus their first info is a MethodType at phase fields.
       */
      sym.isModule || (sym.tpe.params.isEmpty && enteringUncurryIfAtPhaseAfter {
        sym.tpe match {
          case _: NullaryMethodType              => true
          case PolyType(_, _: NullaryMethodType) => true
          case _                                 => false
        }
      })
    }

    /** has this symbol to be translated into a JS setter (both directions)? */
    def isJSSetter(sym: Symbol): Boolean =
      nme.isSetterName(sym.name) && sym.isMethod && !sym.isConstructor

    /** has this symbol to be translated into a JS bracket access (JS to Scala) */
    def isJSBracketAccess(sym: Symbol): Boolean =
      sym.hasAnnotation(JSBracketAccessAnnotation)

    /** has this symbol to be translated into a JS bracket call (JS to Scala) */
    def isJSBracketCall(sym: Symbol): Boolean =
      sym.hasAnnotation(JSBracketCallAnnotation)

    /** Gets the unqualified JS name of a symbol.
     *
     *  If it is not explicitly specified with an `@JSName` annotation, the
     *  JS name is inferred from the Scala name.
     */
    def jsNameOf(sym: Symbol): JSName = {
      sym.getAnnotation(JSNameAnnotation).fold[JSName] {
        JSName.Literal(defaultJSNameOf(sym))
      } { annotation =>
        annotation.constantAtIndex(0).collect {
          case Constant(name: String) => JSName.Literal(name)
        }.getOrElse {
          JSName.Computed(annotation.args.head.symbol)
        }
      }
    }

    def defaultJSNameOf(sym: Symbol): String = {
      val base = sym.unexpandedName.decoded.stripSuffix("_=")
      if (!sym.isMethod) base.stripSuffix(" ")
      else base
    }

    /** Stores the JS native load spec of a symbol for the current compilation
     *  run.
     */
    def storeJSNativeLoadSpec(sym: Symbol, spec: JSNativeLoadSpec): Unit =
      jsNativeLoadSpecs(sym) = spec

    /** Gets the JS native load spec of a symbol in the current compilation run.
     */
    def jsNativeLoadSpecOf(sym: Symbol): JSNativeLoadSpec =
      jsNativeLoadSpecs(sym)

    /** Gets the JS native load spec of a symbol in the current compilation run,
     *  if it has one.
     */
    def jsNativeLoadSpecOfOption(sym: Symbol): Option[JSNativeLoadSpec] =
      jsNativeLoadSpecs.get(sym)

  }

}
