/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package js

/** Encoding of symbol names for JavaScript
 *
 *  @author Sébastien Doeraene
 */
trait JSEncoding extends SubComponent { self: GenJSCode =>
  val global: scalajs.JSGlobal

  import global._

  def environment(implicit pos: Position): js.Ident = {
    js.Ident("$ScalaJSEnvironment")
  }

  def encodeLabelSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(sym.isLabel, "encodeLabelSym called with non-label symbol: " + sym)
    js.Ident("$jslabel$" + sym.name.toString + "$" + sym.id)
  }

  def encodeFieldSym(sym: Symbol)(implicit pos: Position): js.PropertyName = {
    require(sym.owner.isClass && sym.isTerm && !sym.isMethod && !sym.isModule,
        "encodeFieldSym called with non-field symbol: " + sym)

    js.PropertyName("$jsfield$" + sym.name.toString)
  }

  def encodeMethodSym(sym: Symbol)(implicit pos: Position): js.PropertyName = {
    require(sym.isMethod, "encodeMethodSym called with non-method symbol: " + sym)
    js.PropertyName(sym.name.toString + makeParamsString(sym))
  }

  def encodeStaticMemberSym(sym: Symbol)(implicit pos: Position): js.PropertyName = {
    require(sym.isStaticMember,
        "encodeStaticMemberSym called with non-static symbol: " + sym)
    js.PropertyName(sym.name.toString +
        makeParamsString(Nil, sym.tpe))
  }

  def encodeLocalSym(sym: Symbol)(implicit pos: Position): js.Ident = {
    require(!sym.owner.isClass && sym.isTerm && !sym.isMethod && !sym.isModule,
        "encodeLocalSym called with non-local symbol: " + sym)

    if (sym.isValueParameter) js.Ident("arg$" + sym.name.toString)
    else js.Ident(sym.name.toString + "$jsid$" + sym.id)
  }

  def encodeClassSym(sym: Symbol)(implicit pos: Position): js.Tree = {
    require(sym.isClass, "encodeClassSym called with non-class symbol: " + sym)
    js.DotSelect(encodeClassDataOfSym(sym), js.Ident("type"))
  }

  def encodeClassOfType(tpe: Type)(implicit pos: Position): js.Tree = {
    js.DotSelect(encodeClassDataOfType(tpe), js.Ident("class"))
  }

  def encodeModuleSymInternal(sym: Symbol)(implicit pos: Position): js.Tree = {
    require(sym.isModule, "encodeModuleSymInternal called with non-module symbol: " + sym)
    js.DotSelect(encodeModuleDataOfSym(sym), js.Ident("_instance"))
  }

  def encodeModuleSym(sym: Symbol)(implicit pos: Position): js.Tree = {
    require(sym.isModule, "encodeModuleSym called with non-module symbol: " + sym)
    js.DotSelect(encodeModuleDataOfSym(sym), js.Ident("instance"))
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
    import definitions._

    if (sym.isPrimitiveValueClass) {
      val primitiveName = sym match {
        case UnitClass     => "void"
        case BooleanClass  => "boolean"
        case CharClass     => "char"
        case ByteClass     => "byte"
        case ShortClass    => "short"
        case IntClass      => "int"
        case LongClass     => "long"
        case FloatClass    => "float"
        case DoubleClass   => "double"
      }
      js.BracketSelect(js.DotSelect(environment, js.Ident("primitives")),
          js.StringLiteral(primitiveName))
    } else {
      getClassOrModuleData(sym, dictName = "classes")
    }
  }

  def encodeModuleDataOfSym(sym: Symbol)(implicit pos: Position): js.Tree = {
    getClassOrModuleData(sym, dictName = "modules")
  }

  private def getClassOrModuleData(sym: Symbol, dictName: String)(
      implicit pos: Position): js.Tree = {
    js.BracketSelect(js.DotSelect(environment, js.Ident(dictName)),
        js.StringLiteral(encodeFullName(sym)))
  }

  def encodeFullName(sym: Symbol): String =
    sym.fullName + suffixFor(sym)

  def encodeFullName(tpe: Type): String = tpe match {
    case TypeRef(_, definitions.ArrayClass, List(elementType)) =>
      encodeFullName(elementType) + "[]"
    case _ => encodeFullName(tpe.typeSymbol)
  }

  private def suffixFor(sym: Symbol) =
    if (sym.hasModuleFlag && !sym.isMethod && !sym.isImplClass) "$" else ""

  // Encoding of method signatures

  private def makeParamsString(sym: Symbol): String = {
    sym.tpe match {
      case MethodType(params, resultType) =>
        makeParamsString(params map (_.tpe), resultType)

      case NullaryMethodType(resultType) =>
        makeParamsString(Nil, resultType)

      case _ => abort("Expected a method type for " + sym)
    }
  }

  private def makeParamsString(paramTypes: List[Type], resultType: Type): String =
    makeParamsString(paramTypes map typeFullName, typeFullName(resultType))

  private def makeParamsString(paramTypeNames: List[String], resultTypeName: String) =
    paramTypeNames.mkString("(", ",", ")") + ":" + resultTypeName

  private def typeFullName(tpe: Type): String = encodeFullName(tpe)
}
