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

  /** Full name (global name) of the Scala.js environment */
  final val ScalaJSEnvironmentFullName = "$ScalaJSEnvironment"

  /** Name given to the local Scala.js environment variable */
  final val ScalaJSEnvironmentName = "$"

  /** The current Scala.js environment */
  def environment(implicit pos: Position): js.Ident = {
    js.Ident(ScalaJSEnvironmentName)
  }

  /** Select a given field of the current Scala.js environment */
  def envField(name: String)(implicit pos: Position): js.Tree = {
    js.DotSelect(environment, js.Ident(name))
  }

  /** Drop the trailing $ in a string if there is one */
  def dropTrailingDollar(name: String): String =
    if (name.charAt(name.length-1) != '$') name
    else name.substring(0, name.length-1)

  def encodeLabelSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(sym.isLabel, "encodeLabelSym called with non-label symbol: " + sym)
    js.Ident("$jslabel$" + sym.name.toString + "$" + sym.id)
  }

  def encodeFieldSym(sym: Symbol)(implicit pos: Position): js.PropertyName = {
    require(sym.owner.isClass && sym.isTerm && !sym.isMethod && !sym.isModule,
        "encodeFieldSym called with non-field symbol: " + sym)

    val name0 = sym.name.toString
    val name =
      if (name0.charAt(name0.length()-1) != ' ') name0
      else name0.substring(0, name0.length()-1)
    js.PropertyName("$jsfield$" + name)
  }

  def encodeMethodSym(sym: Symbol)(implicit pos: Position): js.PropertyName = {
    require(sym.isMethod, "encodeMethodSym called with non-method symbol: " + sym)
    js.PropertyName(sym.name.toString + makeParamsString(sym))
  }

  def encodeStaticMemberSym(sym: Symbol)(implicit pos: Position): js.PropertyName = {
    require(sym.isStaticMember,
        "encodeStaticMemberSym called with non-static symbol: " + sym)
    js.PropertyName(sym.name.toString +
        makeParamsString(Nil, internalName(sym.tpe)))
  }

  def encodeLocalSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(!sym.owner.isClass && sym.isTerm && !sym.isMethod && !sym.isModule,
        "encodeLocalSym called with non-local symbol: " + sym)

    if (sym.isValueParameter) js.Ident("arg$" + sym.name.toString)
    else js.Ident(sym.name.toString + "$jsid$" + sym.id)
  }

  def encodeClassSym(sym: Symbol)(implicit pos: Position): js.Tree = {
    require(sym.isClass, "encodeClassSym called with non-class symbol: " + sym)
    js.BracketSelect(envField("c"), encodeFullNameLit(sym))
  }

  def encodeClassOfType(tpe: Type)(implicit pos: Position): js.Tree = {
    js.DotSelect(encodeClassDataOfType(tpe), js.Ident("class"))
  }

  def encodeModuleSymInternal(sym: Symbol)(implicit pos: Position): js.Tree = {
    require(sym.isModuleOrModuleClass,
        "encodeModuleSymInternal called with non-module symbol: " + sym)
    js.DotSelect(encodeModuleDataOfSym(sym), js.Ident("_instance"))
  }

  def encodeModuleSym(sym: Symbol)(implicit pos: Position): js.Tree = {
    require(sym.isModuleOrModuleClass,
        "encodeModuleSym called with non-module symbol: " + sym)
    js.BracketSelect(envField("m"), encodeFullNameLit(sym))
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
        js.StringLiteral(encodeFullName(sym)))
  }

  def encodeFullNameLit(sym: Symbol)(implicit pos: Position): js.StringLiteral =
    js.StringLiteral(encodeFullName(sym))

  def encodeFullName(sym: Symbol): String =
    sym.fullName + suffixFor(sym)

  def encodeFullName(tpe: Type): String = tpe match {
    case TypeRef(_, definitions.ArrayClass, List(elementType)) =>
      encodeFullName(elementType) + "[]"
    case _ => encodeFullName(tpe.typeSymbol)
  }

  private def suffixFor(sym: Symbol) =
    if (sym.isModuleClass && !sym.isImplClass) "$" else ""

  // Encoding of method signatures

  private def makeParamsString(sym: Symbol): String = {
    val tpe = sym.tpe
    makeParamsString(tpe.params map (p => internalName(p.tpe)),
        internalName(tpe.resultType))
  }

  private def makeParamsString(paramTypeNames: List[String], resultTypeName: String) =
    paramTypeNames.mkString("(", "", ")") + resultTypeName

  /** Compute the internal name for a type
   *  The internal name is inspired by the encoding of the JVM:
   *  - I for Int, Z for Boolean, V for Unit, etc. for primitive types
   *  - Lclassname; where classname is the full name of a class
   *  - [elem for arrays
   *  but it is tweaked for further compression in the context of Scala.js:
   *  - O for java.lang.Object and T for java.lang.String
   *  - Also we keep . instead of / to separate package names
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
        case definitions.ObjectClass => "O" // or A, for Any?
        case definitions.StringClass => "T" // S is taken, use T for Text
        case _ => "L"+encodeFullName(cls)+";"
      }
    case ARRAY(elem) => "["+internalName(elem)
  }
}
