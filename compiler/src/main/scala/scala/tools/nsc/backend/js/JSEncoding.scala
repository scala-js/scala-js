/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package js

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
  val global: scalajs.JSGlobal

  import global._

  /** Outer separator character */
  final val OuterSep = '\uFE34'

  /** Inner separator character */
  final val InnerSep = '\uFE33'

  /** Outer separator character as a string */
  final val OuterSepStr = OuterSep.toString

  /** Inner separator character as a string */
  final val InnerSepStr = InnerSep.toString

  /** Full name (global name) of the Scala.js environment */
  final val ScalaJSEnvironmentFullName = "ScalaJS"

  /** Name given to the local Scala.js environment variable */
  final val ScalaJSEnvironmentName = "ScalaJS"

  /** The current Scala.js environment */
  def environment(implicit pos: Position): js.Ident = {
    js.Ident(ScalaJSEnvironmentName, Some(ScalaJSEnvironmentFullName))
  }

  /** Select a given field of the current Scala.js environment */
  def envField(name: String)(implicit pos: Position): js.Tree = {
    js.DotSelect(environment, js.Ident(name, Some(name)))
  }

  /** Drop the trailing $ in a string if there is one */
  def dropTrailingDollar(name: String): String =
    if (name.charAt(name.length-1) != '$') name
    else name.substring(0, name.length-1)

  def encodeLabelSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(sym.isLabel, "encodeLabelSym called with non-label symbol: " + sym)
    js.Ident("$jslabel$" + sym.name.toString + "$" + sym.id,
        Some(sym.originalName.decoded))
  }

  def encodeFieldSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(sym.owner.isClass && sym.isTerm && !sym.isMethod && !sym.isModule,
        "encodeFieldSym called with non-field symbol: " + sym)

    val name0 = sym.name.toString
    val name =
      if (name0.charAt(name0.length()-1) != ' ') name0
      else name0.substring(0, name0.length()-1)
    js.Ident("$jsfield$" + name, Some(sym.originalName.decoded))
  }

  def encodeMethodSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(sym.isMethod, "encodeMethodSym called with non-method symbol: " + sym)
    val encodedName =
      if (sym.isClassConstructor) "init" + InnerSep
      else sym.name.toString
    val paramsString = makeParamsString(sym)
    js.Ident(encodedName + paramsString,
        Some(sym.originalName.decoded + paramsString))
  }

  def encodeStaticMemberSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(sym.isStaticMember,
        "encodeStaticMemberSym called with non-static symbol: " + sym)
    js.Ident(
        sym.name.toString + makeParamsString(List(internalName(sym.tpe))),
        Some(sym.originalName.decoded))
  }

  def encodeLocalSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(!sym.owner.isClass && sym.isTerm && !sym.isMethod && !sym.isModule,
        "encodeLocalSym called with non-local symbol: " + sym)

    val origName = Some(sym.originalName.decoded)
    if (sym.isValueParameter) js.Ident("arg$" + sym.name.toString, origName)
    else js.Ident(sym.name.toString + "$jsid$" + sym.id, origName)
  }

  def encodeClassSym(sym: Symbol)(implicit pos: Position): js.Tree = {
    require(sym.isClass, "encodeClassSym called with non-class symbol: " + sym)
    js.DotSelect(envField("c"), encodeFullNameIdent(sym))
  }

  def encodeClassOfType(tpe: Type)(implicit pos: Position): js.Tree = {
    js.DotSelect(encodeClassDataOfType(tpe), js.Ident("cls"))
  }

  def encodeModuleSymInternal(sym: Symbol)(implicit pos: Position): js.Tree = {
    require(sym.isModuleOrModuleClass,
        "encodeModuleSymInternal called with non-module symbol: " + sym)
    js.DotSelect(encodeModuleDataOfSym(sym), js.Ident("_instance"))
  }

  def encodeModuleSym(sym: Symbol)(implicit pos: Position): js.Tree = {
    require(sym.isModuleOrModuleClass,
        "encodeModuleSym called with non-module symbol: " + sym)
    js.DotSelect(envField("m"), encodeFullNameIdent(sym))
  }

  def encodeClassDataOfType(tpe: Type)(implicit pos: Position): js.Tree = {
    toTypeKind(tpe) match {
      case array : ARRAY =>
        var result = encodeClassDataOfSym(array.elementKind.toType.typeSymbol)
        for (i <- 0 until array.dimensions)
          result = js.DotSelect(result, js.Ident("array"))
        result

      case _ => encodeClassDataOfSym(tpe.typeSymbol)
    }
  }

  private def encodeClassDataOfSym(sym: Symbol)(implicit pos: Position): js.Tree = {
    getClassOrModuleData(sym,
        dictName = if (sym.isPrimitiveValueClass) "primitives" else "classes")
  }

  def encodeModuleDataOfSym(sym: Symbol)(implicit pos: Position): js.Tree = {
    getClassOrModuleData(sym, dictName = "modules")
  }

  private def getClassOrModuleData(sym: Symbol, dictName: String)(
      implicit pos: Position): js.Tree = {
    js.BracketSelect(js.DotSelect(environment, js.Ident(dictName)),
        encodeFullNameLit(sym))
  }

  def encodeFullNameIdent(sym: Symbol)(implicit pos: Position): js.Ident = {
    js.Ident(encodeFullName(sym), Some(encodeFullName(sym, '.')))
  }

  def encodeFullNameLit(sym: Symbol)(implicit pos: Position): js.StringLiteral = {
    val encodedFullName = encodeFullName(sym, '.')
    js.StringLiteral(encodedFullName, Some(encodedFullName))
  }

  def encodeFullNameLit(tpe: Type)(implicit pos: Position): js.StringLiteral = {
    val encodedFullName = encodeFullName(tpe, '.')
    js.StringLiteral(encodedFullName, Some(encodedFullName))
  }

  def encodeFullName(sym: Symbol, separator: Char = InnerSep): String =
    sym.fullNameAsName(separator).toString + suffixFor(sym)

  private def encodeFullName(tpe: Type, separator: Char): String = tpe match {
    case TypeRef(_, definitions.ArrayClass, List(elementType)) =>
      encodeFullName(elementType, separator) + "[]"
    case _ => encodeFullName(tpe.typeSymbol, separator)
  }

  private def suffixFor(sym: Symbol) =
    if (sym.isModuleClass && !sym.isImplClass) "$" else ""

  // Encoding of method signatures

  private def makeParamsString(sym: Symbol): String = {
    val tpe = sym.tpe
    val paramTypeNames = tpe.params map (p => internalName(p.tpe))
    makeParamsString(
        if (sym.isClassConstructor) paramTypeNames
        else paramTypeNames :+ internalName(tpe.resultType))
  }

  private def makeParamsString(paramAndResultTypeNames: List[String]) =
    paramAndResultTypeNames.mkString(OuterSepStr, OuterSepStr, "")

  /** Compute the internal name for a type
   *  The internal name is inspired by the encoding of the JVM, with some
   *  tweaks to use only valid JS identifier characters
   *  - I for Int, Z for Boolean, V for Unit, etc. for primitive types
   *  - Lclassname where classname is the full name of a class
   *  - Aelem for arrays
   *  and for further default compression in the context of Scala.js:
   *  - O for java.lang.Object and T for java.lang.String
   *
   *  It might be worth investigating other special cases for classes of the
   *  Scala language: Function types, Tuple types?
   */
  private def internalName(tpe: Type): String = internalName(toTypeKind(tpe))

  private def internalName(kind: TypeKind): String = kind match {
    case kind: ValueTypeKind => kind.primitiveCharCode
    case REFERENCE(cls) =>
      /* Give shorter names to classes used *very* often:
       * - Object, since it is the erasure of most type parameters
       * - String, if only for the ubiquitous toString()
       */
      cls match {
        case definitions.ObjectClass => "O"
        case definitions.StringClass => "T" // S is taken, use T for Text
        case _ => "L"+encodeFullName(cls)
      }
    case ARRAY(elem) => "A"+internalName(elem)
  }
}
