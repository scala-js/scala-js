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
import org.scalajs.ir.{Trees => js, Types => jstpe}
import org.scalajs.ir.Names.{LocalName, LabelName, FieldName, SimpleMethodName, MethodName, ClassName}

import org.scalajs.nscplugin.util.{ScopedVar, VarBox}
import ScopedVar.withScopedVars

/** Encoding of symbol names for the IR. */
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
  final val JSSuperClassParamName = LocalName("superClass$")

  private val xLocalName = LocalName("x")

  private val ScalaRuntimeNullClass = ClassName("scala.runtime.Null$")
  private val ScalaRuntimeNothingClass = ClassName("scala.runtime.Nothing$")

  // Fresh local name generator ----------------------------------------------

  private val usedLocalNames = new ScopedVar[mutable.Set[LocalName]]
  private val localSymbolNames = new ScopedVar[mutable.Map[Symbol, LocalName]]
  private val usedLabelNames = new ScopedVar[mutable.Set[LabelName]]
  private val labelSymbolNames = new ScopedVar[mutable.Map[Symbol, LabelName]]
  private val returnLabelName = new ScopedVar[VarBox[Option[LabelName]]]

  def withNewLocalNameScope[A](body: => A): A = {
    withScopedVars(
        usedLocalNames := mutable.Set.empty,
        localSymbolNames := mutable.Map.empty,
        usedLabelNames := mutable.Set.empty,
        labelSymbolNames := mutable.Map.empty,
        returnLabelName := null
    )(body)
  }

  def reserveLocalName(name: LocalName): Unit = {
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
          js.Labeled(js.LabelIdent(labelName), tpe, inner)
      }
    }
  }

  private def freshNameGeneric[N <: ir.Names.Name](base: N,
      usedNamesSet: mutable.Set[N])(
      withSuffix: (N, String) => N): N = {

    var suffix = 1
    var result = base
    while (usedNamesSet(result)) {
      suffix += 1
      result = withSuffix(base, "$" + suffix)
    }
    usedNamesSet += result
    result
  }

  private def freshName(base: LocalName): LocalName =
    freshNameGeneric(base, usedLocalNames)(_.withSuffix(_))

  private def freshName(base: String): LocalName =
    freshName(LocalName(base))

  def freshLocalIdent()(implicit pos: ir.Position): js.LocalIdent =
    js.LocalIdent(freshName(xLocalName), None)

  def freshLocalIdent(base: LocalName)(implicit pos: ir.Position): js.LocalIdent = {
    val fresh = freshName(base)
    js.LocalIdent(fresh, if (fresh eq base) None else Some(base.nameString))
  }

  def freshLocalIdent(base: String)(implicit pos: ir.Position): js.LocalIdent =
    freshLocalIdent(LocalName(base))

  private def localSymbolName(sym: Symbol): LocalName = {
    localSymbolNames.getOrElseUpdate(sym, {
      /* The emitter does not like local variables that start with a '$',
       * because it needs to encode them not to clash with emitter-generated
       * names. There are two common cases, caused by scalac-generated names:
       * - the `$this` parameter of tailrec methods and "extension" methods of
       *   AnyVals, which scalac knows as `nme.SELF`, and
       * - the `$outer` parameter of inner class constructors, which scalac
       *   knows as `nme.OUTER`.
       * We choose different base names for those two cases instead, so that
       * the avoidance mechanism of the emitter doesn't happen as a common
       * case. It can still happen for user-defined variables, but in that case
       * the emitter will deal with it.
       */
      val base = sym.name match {
        case nme.SELF  => "this$" // instead of $this
        case nme.OUTER => "outer" // instead of $outer
        case name      => name.toString()
      }
      freshName(base)
    })
  }

  private def freshLabelName(base: LabelName): LabelName =
    freshNameGeneric(base, usedLabelNames)(_.withSuffix(_))

  private def freshLabelName(base: String): LabelName =
    freshLabelName(LabelName(base))

  def freshLabelIdent(base: String)(implicit pos: ir.Position): js.LabelIdent =
    js.LabelIdent(freshLabelName(base))

  private def labelSymbolName(sym: Symbol): LabelName =
    labelSymbolNames.getOrElseUpdate(sym, freshLabelName(sym.name.toString))

  def getEnclosingReturnLabel()(implicit pos: ir.Position): js.LabelIdent = {
    val box = returnLabelName.get
    if (box == null)
      throw new IllegalStateException(s"No enclosing returnable scope at $pos")
    if (box.value.isEmpty)
      box.value = Some(freshLabelName("_return"))
    js.LabelIdent(box.value.get)
  }

  // Encoding methods ----------------------------------------------------------

  def encodeLabelSym(sym: Symbol)(implicit pos: Position): js.LabelIdent = {
    require(sym.isLabel, "encodeLabelSym called with non-label symbol: " + sym)
    js.LabelIdent(labelSymbolName(sym))
  }

  def encodeFieldSym(sym: Symbol)(implicit pos: Position): js.FieldIdent = {
    requireSymIsField(sym)
    val name = sym.name.dropLocal
    js.FieldIdent(FieldName(name.toString()), originalNameOf(name))
  }

  def encodeFieldSymAsStringLiteral(sym: Symbol)(
      implicit pos: Position): js.StringLiteral = {

    requireSymIsField(sym)
    js.StringLiteral(sym.name.dropLocal.toString())
  }

  private def requireSymIsField(sym: Symbol): Unit = {
    require(sym.owner.isClass && sym.isTerm && !sym.isMethod && !sym.isModule,
        "encodeFieldSym called with non-field symbol: " + sym)
  }

  def encodeMethodSym(sym: Symbol, reflProxy: Boolean = false)(
      implicit pos: Position): js.MethodIdent = {

    require(sym.isMethod,
        "encodeMethodSym called with non-method symbol: " + sym)

    val tpe = sym.tpe

    val paramTypeRefs0 = tpe.params.map(p => paramOrResultTypeRef(p.tpe))

    val hasExplicitThisParameter = isNonNativeJSClass(sym.owner)
    val paramTypeRefs =
      if (!hasExplicitThisParameter) paramTypeRefs0
      else paramOrResultTypeRef(sym.owner.toTypeConstructor) :: paramTypeRefs0

    val resultTypeRef =
      if (sym.isClassConstructor || reflProxy) None
      else Some(paramOrResultTypeRef(tpe.resultType))

    val name = sym.name
    val simpleName = SimpleMethodName(name.toString())
    val methodName = MethodName(simpleName, paramTypeRefs, resultTypeRef)

    js.MethodIdent(methodName, originalNameOf(name))
  }

  def encodeStaticMemberSym(sym: Symbol)(implicit pos: Position): js.MethodIdent = {
    require(sym.isStaticMember,
        "encodeStaticMemberSym called with non-static symbol: " + sym)

    val name = sym.name
    val resultTypeRef = paramOrResultTypeRef(sym.tpe)
    val methodName = MethodName(name.toString(), Nil, resultTypeRef)
    js.MethodIdent(methodName, originalNameOf(name))
  }

  /** Computes the internal name for a type. */
  private def paramOrResultTypeRef(tpe: Type): jstpe.TypeRef = {
    toTypeRef(tpe) match {
      case jstpe.ClassRef(ScalaRuntimeNothingClass) => jstpe.NothingRef
      case jstpe.ClassRef(ScalaRuntimeNullClass)    => jstpe.NullRef
      case typeRef                                  => typeRef
    }
  }

  def encodeLocalSym(sym: Symbol)(implicit pos: Position): js.LocalIdent = {
    /* The isValueParameter case is necessary to work around an internal bug
     * of scalac: for some @varargs methods, the owner of some parameters is
     * the enclosing class rather the method, so !sym.owner.isClass fails.
     * Go figure ...
     * See #1440
     */
    require(sym.isValueParameter ||
        (!sym.owner.isClass && sym.isTerm && !sym.isMethod && !sym.isModule),
        "encodeLocalSym called with non-local symbol: " + sym)
    js.LocalIdent(localSymbolName(sym), Some(sym.unexpandedName.decoded))
  }

  def foreignIsImplClass(sym: Symbol): Boolean =
    sym.isModuleClass && nme.isImplClassName(sym.name)

  def encodeClassType(sym: Symbol): jstpe.Type = {
    if (sym == definitions.ObjectClass) jstpe.AnyType
    else if (isJSType(sym)) jstpe.AnyType
    else {
      assert(sym != definitions.ArrayClass,
          "encodeClassType() cannot be called with ArrayClass")
      jstpe.ClassType(encodeClassName(sym))
    }
  }

  def encodeClassNameIdent(sym: Symbol)(implicit pos: Position): js.ClassIdent =
    js.ClassIdent(encodeClassName(sym), Some(sym.fullName))

  private val BoxedStringModuleClassName = ClassName("java.lang.String$")
  private val BoxedVoidModuleClassName = ClassName("java.lang.Void$")

  def encodeClassName(sym: Symbol): ClassName = {
    assert(!sym.isPrimitiveValueClass,
        s"Illegal encodeClassName(${sym.fullName}")
    if (sym == jsDefinitions.HackedStringClass) {
      ir.Names.BoxedStringClass
    } else if (sym == jsDefinitions.HackedStringModClass) {
      BoxedStringModuleClassName
    } else if (sym == definitions.BoxedUnitClass) {
      // Rewire scala.runtime.BoxedUnit to java.lang.Void, as the IR expects
      // BoxedUnit$ is a JVM artifact
      ir.Names.BoxedUnitClass
    } else if (sym == jsDefinitions.BoxedUnitModClass) {
      // Same for its module class
      BoxedVoidModuleClassName
    } else {
      ClassName(sym.fullName + (if (needsModuleClassSuffix(sym)) "$" else ""))
    }
  }

  def needsModuleClassSuffix(sym: Symbol): Boolean =
    sym.isModuleClass && !foreignIsImplClass(sym)

  private def originalNameOf(name: Name): Option[String] = {
    val originalName = nme.unexpandedName(name).decoded
    if (originalName == name.toString) None
    else Some(originalName)
  }
}
