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

import scala.tools.nsc._

import org.scalajs.ir
import ir.{Trees => js, Types => jstpe}

import org.scalajs.nscplugin.util.{ScopedVar, VarBox}
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
trait JSEncoding[G <: Global with Singleton] extends SubComponent {
  self: GenJSCode[G] =>

  import global._
  import jsAddons._

  /** Name of the capture param storing the JS super class.
   *
   *  This is used by the dispatchers of exposed JS methods and properties of
   *  nested JS classes when they need to perform a super call. Other super
   *  calls (in the actual bodies of the methods, not in the dispatchers) do
   *  not use this value, since they are implemented as static methods that do
   *  not have access to it. Instead, they get the JS super class value through
   *  the magic method inserted by `ExplicitLocalJS`, leveraging `lambdalift`
   *  to ensure that it is properly captured.
   *
   *  Using this identifier is only allowed if it was reserved in the current
   *  local name scope using [[reserveLocalName]]. Otherwise, this name can
   *  clash with another local identifier.
   */
  final val JSSuperClassParamName = "$superClass"

  // Fresh local name generator ----------------------------------------------

  private val usedLocalNames = new ScopedVar[mutable.Set[String]]
  private val returnLabelName = new ScopedVar[VarBox[Option[String]]]
  private val localSymbolNames = new ScopedVar[mutable.Map[Symbol, String]]
  private val isReserved = Set("arguments", "eval")

  def withNewLocalNameScope[A](body: => A): A = {
    withScopedVars(
        usedLocalNames := mutable.Set.empty,
        returnLabelName := null,
        localSymbolNames := mutable.Map.empty
    )(body)
  }

  def reserveLocalName(name: String): Unit = {
    require(usedLocalNames.isEmpty,
        s"Trying to reserve the name '$name' but names have already been " +
        "allocated")
    usedLocalNames += name
  }

  def withNewReturnableScope(tpe: jstpe.Type)(body: => js.Tree)(
      implicit pos: ir.Position): js.Tree = {
    withScopedVars(
        returnLabelName := new VarBox(None)
    ) {
      val inner = body
      returnLabelName.get.value match {
        case None =>
          inner
        case Some(labelName) =>
          js.Labeled(js.Ident(labelName), tpe, inner)
      }
    }
  }

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

  def getEnclosingReturnLabel()(implicit pos: ir.Position): js.Ident = {
    val box = returnLabelName.get
    if (box == null)
      throw new IllegalStateException(s"No enclosing returnable scope at $pos")
    if (box.value.isEmpty)
      box.value = Some(freshName("_return"))
    js.Ident(box.value.get)
  }

  // Encoding methods ----------------------------------------------------------

  def encodeLabelSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(sym.isLabel, "encodeLabelSym called with non-label symbol: " + sym)
    js.Ident(localSymbolName(sym), Some(sym.unexpandedName.decoded))
  }

  def encodeFieldSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(sym.owner.isClass && sym.isTerm && !sym.isMethod && !sym.isModule,
        "encodeFieldSym called with non-field symbol: " + sym)

    val name0 = encodeMemberNameInternal(sym)
    val name =
      if (name0.charAt(name0.length()-1) != ' ') name0
      else name0.substring(0, name0.length()-1)

    js.Ident(mangleJSName(name), Some(sym.unexpandedName.decoded))
  }

  def encodeMethodSym(sym: Symbol, reflProxy: Boolean = false)(
      implicit pos: Position): js.Ident = {

    require(sym.isMethod,
        "encodeMethodSym called with non-method symbol: " + sym)

    val encodedName =
      if (sym.isClassConstructor) "init_"
      else mangleJSName(encodeMemberNameInternal(sym))

    val paramsString = makeParamsString(sym, reflProxy)

    js.Ident(encodedName + paramsString,
        Some(sym.unexpandedName.decoded + paramsString))
  }

  def encodeStaticMemberSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(sym.isStaticMember,
        "encodeStaticMemberSym called with non-static symbol: " + sym)
    js.Ident(
        mangleJSName(encodeMemberNameInternal(sym)) + "__" + internalName(sym.tpe),
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
    else if (isJSType(sym)) jstpe.AnyType
    else {
      assert(sym != definitions.ArrayClass,
          "encodeClassType() cannot be called with ArrayClass")
      jstpe.ClassType(encodeClassFullName(sym))
    }
  }

  def encodeClassRef(sym: Symbol): jstpe.ClassRef =
    jstpe.ClassRef(encodeClassFullName(sym))

  def encodeClassFullNameIdent(sym: Symbol)(implicit pos: Position): js.Ident = {
    js.Ident(encodeClassFullName(sym), Some(sym.fullName))
  }

  def encodeClassFullName(sym: Symbol): String = {
    assert(!sym.isPrimitiveValueClass,
        s"Illegal encodeClassFullName(${sym.fullName}")
    if (sym == jsDefinitions.HackedStringClass) {
      ir.Definitions.BoxedStringClass
    } else if (sym == jsDefinitions.HackedStringModClass) {
      "jl_String$"
    } else if (sym == definitions.BoxedUnitClass) {
      // Rewire scala.runtime.BoxedUnit to java.lang.Void, as the IR expects
      // BoxedUnit$ is a JVM artifact
      ir.Definitions.BoxedUnitClass
    } else if (sym == jsDefinitions.BoxedUnitModClass) {
      // Same for its module class
      "jl_Void$"
    } else {
      ir.Definitions.encodeClassName(
          sym.fullName + (if (needsModuleClassSuffix(sym)) "$" else ""))
    }
  }

  def needsModuleClassSuffix(sym: Symbol): Boolean =
    sym.isModuleClass && !foreignIsImplClass(sym)

  private def encodeMemberNameInternal(sym: Symbol): String =
    sym.name.toString.replace("_", "$und")

  // Encoding of method signatures

  private def makeParamsString(sym: Symbol, reflProxy: Boolean): String = {
    val tpe = sym.tpe

    val paramTypeNames0 = tpe.params map (p => internalName(p.tpe))

    val hasExplicitThisParameter = isNonNativeJSClass(sym.owner)
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
    paramAndResultTypeNames.mkString("__", "__", "")
  }

  /** Computes the internal name for a type. */
  private def internalName(tpe: Type): String = {
    val patchedTypeRef = toTypeRef(tpe) match {
      case jstpe.ClassRef("sr_Nothing$") => jstpe.NothingRef
      case jstpe.ClassRef("sr_Null$")    => jstpe.NullRef
      case typeRef                       => typeRef
    }
    ir.Definitions.encodeTypeRef(patchedTypeRef)
  }

  /** mangles names that are illegal in JavaScript by prepending a $
   *  also mangles names that would collide with these mangled names
   */
  private def mangleJSName(name: String) =
    if (js.isKeyword(name) || name(0).isDigit || name(0) == '$')
      "$" + name
    else name
}
