/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.collection.mutable

import scala.tools.nsc._

import scala.scalajs.ir
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
  private val isKeywordOrReserved =
    js.isKeyword ++ Seq("arguments", ScalaJSEnvironmentName)

  def withNewLocalNameScope[A](body: => A): A =
    withScopedVars(
        usedLocalNames := mutable.Set.empty,
        localSymbolNames := mutable.Map.empty
    )(body)

  private def freshName(base: String = "x"): String = {
    var suffix = 1
    var longName = base
    while (usedLocalNames(longName) || isKeywordOrReserved(longName)) {
      suffix += 1
      longName = base+"$"+suffix
    }
    usedLocalNames += longName
    longName
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

  private lazy val allRefClasses: Set[Symbol] = {
    import definitions._
    (Set(ObjectRefClass, VolatileObjectRefClass) ++
        refClass.values ++ volatileRefClass.values)
  }

  def encodeFieldSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(sym.owner.isClass && sym.isTerm && !sym.isMethod && !sym.isModule,
        "encodeFieldSym called with non-field symbol: " + sym)

    val name0 = encodeMemberNameInternal(sym)
    val name =
      if (name0.charAt(name0.length()-1) != ' ') name0
      else name0.substring(0, name0.length()-1)

    /* We have to special-case fields of Ref types (IntRef, ObjectRef, etc.)
     * because they are emitted as private by our .scala source files, but
     * they are considered public at use site since their symbols come from
     * Java-emitted .class files.
     */
    val idSuffix =
      if (sym.isPrivate || allRefClasses.contains(sym.owner))
        sym.owner.ancestors.count(!_.isInterface).toString
      else
        "f"

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
      reflProxy: Boolean = false): (String, String) = {
    require(sym.isMethod, "encodeMethodSym called with non-method symbol: " + sym)

    def name = encodeMemberNameInternal(sym)

    val encodedName = {
      if (sym.isClassConstructor)
        "init" + InnerSep
      else if (foreignIsImplClass(sym.owner))
        encodeClassFullName(sym.owner) + OuterSep + name
      else if (sym.isPrivate)
        mangleJSName(name) + OuterSep + "p" +
          sym.owner.ancestors.count(!_.isInterface).toString
      else
        mangleJSName(name)
    }

    val paramsString = makeParamsString(sym, reflProxy)

    (encodedName, paramsString)
  }

  /** encode a method symbol on a trait so it refers to its corresponding
   *  symbol in the implementation class. This works around a limitation in
   *  Scalac 2.11 that doesn't return members for the implementation class.
   */
  def encodeImplClassMethodSym(sym: Symbol)(
      implicit pos: Position): (Symbol, js.Ident) = {
    require(sym.owner.isInterface)
    // Unfortunately we cannot verify here, whether this symbol is actually
    // implemented in the trait.

    // Prepare special parameter string
    val tpe = sym.tpe
    val paramTpes = sym.owner.tpe +: tpe.params.map(_.tpe) :+ tpe.resultType
    val paramsString = makeParamsString(paramTpes.map(internalName _))

    // Encode prefix
    val implClass = sym.owner.implClass orElse erasure.implClass(sym.owner)
    val encodedName =
      encodeClassFullName(implClass) + OuterSep + encodeMemberNameInternal(sym)

    // Create ident
    (implClass, js.Ident(encodedName + paramsString,
        Some(sym.unexpandedName.decoded + paramsString)))
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
    require(!sym.owner.isClass && sym.isTerm && !sym.isMethod && !sym.isModule,
        "encodeLocalSym called with non-local symbol: " + sym)
    js.Ident(mangleJSName(localSymbolName(sym)), Some(sym.unexpandedName.decoded))
  }

  def foreignIsImplClass(sym: Symbol): Boolean =
    sym.isModuleClass && nme.isImplClassName(sym.name)

  def encodeReferenceType(tpe: Type)(implicit pos: Position): (jstpe.ReferenceType, Symbol) = {
    toTypeKind(tpe) match {
      case kind: ARRAY =>
        val sym = mapRuntimeClass(kind.elementKind.toType.typeSymbol)
        (jstpe.ArrayType(encodeClassFullName(sym), kind.dimensions), sym)

      case kind =>
        val sym = mapRuntimeClass(kind.toType.typeSymbol)
        (jstpe.ClassType(encodeClassFullName(sym)), sym)
    }
  }

  def encodeArrayType(tpe: Type)(implicit pos: Position): (jstpe.ArrayType, Symbol) = {
    (encodeReferenceType(tpe): @unchecked) match {
      case (arrayType: jstpe.ArrayType, sym) => (arrayType, sym)
    }
  }

  def encodeClassType(sym: Symbol): jstpe.Type = {
    if (sym == definitions.ObjectClass) jstpe.AnyType
    else if (isRawJSType(sym.toTypeConstructor)) jstpe.DynType
    else {
      assert(sym != definitions.ArrayClass,
          "encodeClassType() cannot be called with ArrayClass")
      jstpe.ClassType(encodeClassFullName(sym))
    }
  }

  def encodeClassFullNameIdent(sym: Symbol)(implicit pos: Position): js.Ident = {
    js.Ident(encodeClassFullName(sym), Some(sym.fullName))
  }

  def encodeModuleFullNameIdent(sym: Symbol)(implicit pos: Position): js.Ident = {
    js.Ident(encodeModuleFullName(sym), Some(sym.fullName))
  }

  def encodeClassFullName(sym: Symbol): String = {
    ir.Definitions.encodeClassName(
        sym.fullName + (if (needsModuleClassSuffix(sym)) "$" else ""))
  }

  def needsModuleClassSuffix(sym: Symbol): Boolean =
    sym.isModuleClass && !foreignIsImplClass(sym)

  def encodeModuleFullName(sym: Symbol): String =
    ir.Definitions.encodeClassName(sym.fullName + "$").dropRight(1)

  private def encodeMemberNameInternal(sym: Symbol): String =
    sym.name.toString.replace("_", "$und")

  // Encoding of method signatures

  private def makeParamsString(sym: Symbol, reflProxy: Boolean): String = {
    val tpe = sym.tpe
    val paramTypeNames = tpe.params map (p => internalName(p.tpe))
    makeParamsString(
        if (sym.isClassConstructor)
          paramTypeNames
        else if (reflProxy)
          paramTypeNames :+ ""
        else
          paramTypeNames :+ internalName(tpe.resultType))
  }

  private def makeParamsString(paramAndResultTypeNames: List[String]) =
    paramAndResultTypeNames.mkString(OuterSep, OuterSep, "")

  /** Computes the internal name for a type. */
  private def internalName(tpe: Type): String = internalName(toTypeKind(tpe))

  private def internalName(kind: TypeKind): String = kind match {
    case kind: ValueTypeKind => kind.primitiveCharCode
    case REFERENCE(cls)      => encodeClassFullName(mapRuntimeClass(cls))
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
