/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package org.scalajs.core.compiler

import scala.collection.mutable

import scala.tools.nsc._

import org.scalajs.core.ir
import ir.{Trees => js, Types => jstpe}

import util.ScopedVar
import ScopedVar.withScopedVars

/** Encoding of symbol names for JavaScript
 *
 *  Some issues that this encoding solves:
 *  * Overloading: encode the full signature in the JS name
 *  * Same scope for fields and methods of a class
 *  * Global access to classes and modules (by their full name)
 *
 *  @author Sébastien Doeraene
 */
trait JSEncoding extends SubComponent { self: GenJSCode =>
  import global._
  import jsAddons._

  /** Outer separator string (between parameter types) */
  final val OuterSep = "__"

  /** Inner separator character (replace dots in full names) */
  final val InnerSep = "_"

  /** Name given to the local Scala.js environment variable */
  final val ScalaJSEnvironmentName = "ScalaJS"

  /** Name given to all exported stuff of a class for DCE */
  final val dceExportName = "<exported>"

  // Fresh local name generator ----------------------------------------------

  private val usedLocalNames = new ScopedVar[mutable.Set[String]]
  private val localSymbolNames = new ScopedVar[mutable.Map[Symbol, String]]
  private val isReserved =
    Set("arguments", "eval", ScalaJSEnvironmentName)

  def withNewLocalNameScope[A](body: => A): A =
    withScopedVars(
        usedLocalNames := mutable.Set.empty,
        localSymbolNames := mutable.Map.empty
    )(body)

  private def freshName(base: String = "x"): String = {
    var suffix = 1
    var longName = base
    while (usedLocalNames(longName) || isReserved(longName)) {
      suffix += 1
      longName = base+"$"+suffix
    }
    usedLocalNames += longName
    mangleJSName(longName)
  }

  def freshLocalIdent()(implicit pos: ir.Position): js.Ident =
    js.Ident(freshName(), None)

  def freshLocalIdent(base: String)(implicit pos: ir.Position): js.Ident =
    js.Ident(freshName(base), Some(base))

  private def localSymbolName(sym: Symbol): String =
    localSymbolNames.getOrElseUpdate(sym, freshName(sym.name.toString))

  // Encoding methods ----------------------------------------------------------

  def encodeLabelSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(sym.isLabel, "encodeLabelSym called with non-label symbol: " + sym)
    js.Ident(localSymbolName(sym), Some(sym.unexpandedName.decoded))
  }

  /** See comment in `encodeFieldSym()`. */
  private lazy val shouldMangleOuterPointerName = {
    val v = scala.util.Properties.versionNumberString
    !(v.startsWith("2.10.") || v.startsWith("2.11.") || v == "2.12.0-RC1")
  }

  def encodeFieldSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(sym.owner.isClass && sym.isTerm && !sym.isMethod && !sym.isModule,
        "encodeFieldSym called with non-field symbol: " + sym)

    val name0 = encodeMemberNameInternal(sym)
    val name =
      if (name0.charAt(name0.length()-1) != ' ') name0
      else name0.substring(0, name0.length()-1)

    /* Java-defined fields are always accessed as if they were private. This
     * is necessary because they are defined as private by our .scala source
     * files, but they are considered `!isPrivate` at use site, since their
     * symbols come from Java-emitted .class files. Fortunately, we can
     * easily detect those as `isJavaDefined`. This includes fields of Ref
     * types (IntRef, ObjectRef, etc.) which were special-cased at use-site
     * in Scala.js < 0.6.15.
     * Caveat: because of this, changing the length of the superclass chain of
     * a Java-defined class is a binary incompatible change.
     *
     * Starting with 2.12.0-RC2, we also special case outer fields. This
     * essentially fixes #2382, which is caused by a class having various $outer
     * pointers in its hierarchy that points to different outer instances.
     * Without this fix, they all collapse to the same field in the IR. We
     * cannot fix this for all Scala versions at the moment, because that would
     * break backwards binary compatibility. We *do* fix it for 2.12.0-RC2
     * onwards because that also fixes #2625, which surfaced in 2.12 and is
     * therefore a regression. We can do this because the 2.12 ecosystem is
     * not binary compatible anyway (because of Scala) so we can break it on
     * our side at the same time.
     *
     * TODO We should probably consider emitting *all* fields with an ancestor
     * count. We cannot do that in a binary compatible way, though. This is
     * filed as #2629.
     */
    val idSuffix: String = {
      val usePerClassSuffix = {
        sym.isPrivate ||
        sym.isJavaDefined ||
        (shouldMangleOuterPointerName && sym.isOuterField)
      }
      if (usePerClassSuffix)
        sym.owner.ancestors.count(!_.isTraitOrInterface).toString
      else
        "f"
    }

    val encodedName = name + "$" + idSuffix
    js.Ident(mangleJSName(encodedName), Some(sym.unexpandedName.decoded))
  }

  def encodeMethodSym(sym: Symbol, reflProxy: Boolean = false)
                     (implicit pos: Position): js.Ident = {
    val (encodedName, paramsString) = encodeMethodNameInternal(sym, reflProxy)
    js.Ident(encodedName + paramsString,
        Some(sym.unexpandedName.decoded + paramsString))
  }

  def encodeMethodName(sym: Symbol, reflProxy: Boolean = false): String = {
    val (encodedName, paramsString) = encodeMethodNameInternal(sym, reflProxy)
    encodedName + paramsString
  }

  /** Encodes a method symbol of java.lang.String for use in RuntimeString.
   *
   *  This basically means adding an initial parameter of type
   *  java.lang.String, which is the `this` parameter.
   */
  def encodeRTStringMethodSym(sym: Symbol)(
      implicit pos: Position): (Symbol, js.Ident) = {
    require(sym.isMethod, "encodeMethodSym called with non-method symbol: " + sym)
    require(sym.owner == definitions.StringClass)
    require(!sym.isClassConstructor && !sym.isPrivate)

    val (encodedName, paramsString) =
      encodeMethodNameInternal(sym, inRTClass = true)
    val methodIdent = js.Ident(encodedName + paramsString,
        Some(sym.unexpandedName.decoded + paramsString))

    (jsDefinitions.RuntimeStringModuleClass, methodIdent)
  }

  private def encodeMethodNameInternal(sym: Symbol,
      reflProxy: Boolean = false,
      inRTClass: Boolean = false): (String, String) = {
    require(sym.isMethod, "encodeMethodSym called with non-method symbol: " + sym)

    def name = encodeMemberNameInternal(sym)

    def privateSuffix(owner: Symbol): String =
      if (owner.isTraitOrInterface && !owner.isImplClass) encodeClassFullName(owner)
      else owner.ancestors.count(!_.isTraitOrInterface).toString

    val encodedName = {
      if (sym.isClassConstructor)
        "init" + InnerSep
      else if (sym.isPrivate)
        mangleJSName(name) + OuterSep + "p" + privateSuffix(sym.owner)
      else
        mangleJSName(name)
    }

    val paramsString = makeParamsString(sym, reflProxy, inRTClass)

    (encodedName, paramsString)
  }

  def encodeStaticMemberSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(sym.isStaticMember,
        "encodeStaticMemberSym called with non-static symbol: " + sym)
    js.Ident(
        mangleJSName(encodeMemberNameInternal(sym)) +
        makeParamsString(List(internalName(sym.tpe))),
        Some(sym.unexpandedName.decoded))
  }

  def encodeLocalSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    /* The isValueParameter case is necessary to work around an internal bug
     * of scalac: for some @varargs methods, the owner of some parameters is
     * the enclosing class rather the method, so !sym.owner.isClass fails.
     * Go figure ...
     * See #1440
     */
    require(sym.isValueParameter ||
        (!sym.owner.isClass && sym.isTerm && !sym.isMethod && !sym.isModule),
        "encodeLocalSym called with non-local symbol: " + sym)
    js.Ident(localSymbolName(sym), Some(sym.unexpandedName.decoded))
  }

  def foreignIsImplClass(sym: Symbol): Boolean =
    sym.isModuleClass && nme.isImplClassName(sym.name)

  def encodeClassType(sym: Symbol): jstpe.Type = {
    if (sym == definitions.ObjectClass) jstpe.AnyType
    else if (isRawJSType(sym.toTypeConstructor)) jstpe.AnyType
    else {
      assert(sym != definitions.ArrayClass,
          "encodeClassType() cannot be called with ArrayClass")
      jstpe.ClassType(encodeClassFullName(sym))
    }
  }

  def encodeClassFullNameIdent(sym: Symbol)(implicit pos: Position): js.Ident = {
    js.Ident(encodeClassFullName(sym), Some(sym.fullName))
  }

  def encodeClassFullName(sym: Symbol): String = {
    ir.Definitions.encodeClassName(
        sym.fullName + (if (needsModuleClassSuffix(sym)) "$" else ""))
  }

  def needsModuleClassSuffix(sym: Symbol): Boolean =
    sym.isModuleClass && !foreignIsImplClass(sym)

  def encodeComputedNameIdentity(sym: Symbol): String = {
    assert(sym.owner.isModuleClass)
    encodeClassFullName(sym.owner) + "__" + encodeMemberNameInternal(sym)
  }

  private def encodeMemberNameInternal(sym: Symbol): String =
    sym.name.toString.replace("_", "$und")

  // Encoding of method signatures

  private def makeParamsString(sym: Symbol, reflProxy: Boolean,
      inRTClass: Boolean): String = {
    val tpe = sym.tpe

    val paramTypeNames0 = tpe.params map (p => internalName(p.tpe))

    val hasExplicitThisParameter =
      inRTClass || isScalaJSDefinedJSClass(sym.owner)
    val paramTypeNames =
      if (!hasExplicitThisParameter) paramTypeNames0
      else internalName(sym.owner.toTypeConstructor) :: paramTypeNames0

    val paramAndResultTypeNames = {
      if (sym.isClassConstructor)
        paramTypeNames
      else if (reflProxy)
        paramTypeNames :+ ""
      else
        paramTypeNames :+ internalName(tpe.resultType)
    }
    makeParamsString(paramAndResultTypeNames)
  }

  private def makeParamsString(paramAndResultTypeNames: List[String]) =
    paramAndResultTypeNames.mkString(OuterSep, OuterSep, "")

  /** Computes the internal name for a type. */
  private def internalName(tpe: Type): String = internalName(toTypeKind(tpe))

  private def internalName(kind: TypeKind): String = kind match {
    case VOID                => "V"
    case kind: ValueTypeKind => kind.primitiveCharCode.toString()
    case NOTHING             => ir.Definitions.RuntimeNothingClass
    case NULL                => ir.Definitions.RuntimeNullClass
    case REFERENCE(cls)      => encodeClassFullName(cls)
    case ARRAY(elem)         => "A"+internalName(elem)
  }

  /** mangles names that are illegal in JavaScript by prepending a $
   *  also mangles names that would collide with these mangled names
   */
  private def mangleJSName(name: String) =
    if (js.isKeyword(name) || name(0).isDigit || name(0) == '$')
      "$" + name
    else name
}
