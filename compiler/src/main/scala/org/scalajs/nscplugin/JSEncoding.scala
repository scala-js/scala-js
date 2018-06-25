/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
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
   *  Using this identifier is only allowed if the current local name scope was
   *  created with [[withNewLocalNameScopeUsingJSSuperClassParamName]].
   *  Otherwise, this name can clash with another local identifier.
   */
  final val JSSuperClassParamName = "$superClass"

  // Fresh local name generator ----------------------------------------------

  private val usedLocalNames = new ScopedVar[mutable.Set[String]]
  private val returnLabelName = new ScopedVar[VarBox[Option[String]]]
  private val localSymbolNames = new ScopedVar[mutable.Map[Symbol, String]]
  private val isReserved =
    Set("arguments", "eval", ScalaJSEnvironmentName)

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
     * We also special case outer fields. This essentially fixes #2382, which
     * is caused by a class having various $outer pointers in its hierarchy
     * that point to different outer instances. Without this fix, they all
     * collapse to the same field in the IR.
     *
     * TODO We should probably consider emitting *all* fields with an ancestor
     * count. We cannot do that in a binary compatible way, though. This is
     * filed as #2629.
     */
    val idSuffix: String = {
      val usePerClassSuffix = {
        sym.isPrivate ||
        sym.isJavaDefined ||
        sym.isOuterField
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

  private def encodeMethodNameInternal(sym: Symbol,
      reflProxy: Boolean): (String, String) = {
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

    val paramsString = makeParamsString(sym, reflProxy)

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
    else if (isJSType(sym)) jstpe.AnyType
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
    assert(!sym.isPrimitiveValueClass,
        s"Illegal encodeClassFullName(${sym.fullName}")
    if (sym == jsDefinitions.HackedStringClass) {
      ir.Definitions.BoxedStringClass
    } else if (sym == jsDefinitions.HackedStringModClass) {
      "jl_String$"
    } else {
      ir.Definitions.encodeClassName(
          sym.fullName + (if (needsModuleClassSuffix(sym)) "$" else ""))
    }
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
    makeParamsString(paramAndResultTypeNames)
  }

  private def makeParamsString(paramAndResultTypeNames: List[String]) =
    paramAndResultTypeNames.mkString(OuterSep, OuterSep, "")

  /** Computes the internal name for a type. */
  private def internalName(tpe: Type): String = internalName(toTypeKind(tpe))

  private def internalName(kind: TypeKind): String = kind match {
    case VOID                => "V"
    case kind: ValueTypeKind => kind.primitiveCharCode.toString()
    case NOTHING             => ir.Definitions.NothingClass
    case NULL                => ir.Definitions.NullClass
    case REFERENCE(cls)      => encodeClassFullName(cls)
    case ARRAY(elem)         => "A" + internalArrayElemName(elem)
  }

  private def internalArrayElemName(kind: TypeKind): String = kind match {
    case VOID                => "V"
    case kind: ValueTypeKind => kind.primitiveCharCode.toString()
    case NOTHING             => encodeClassFullName(definitions.RuntimeNothingClass)
    case NULL                => encodeClassFullName(definitions.RuntimeNullClass)
    case REFERENCE(cls)      => encodeClassFullName(cls)
    case ARRAY(elem)         => "A" + internalArrayElemName(elem)
  }

  /** mangles names that are illegal in JavaScript by prepending a $
   *  also mangles names that would collide with these mangled names
   */
  private def mangleJSName(name: String) =
    if (js.isKeyword(name) || name(0).isDigit || name(0) == '$')
      "$" + name
    else name
}
