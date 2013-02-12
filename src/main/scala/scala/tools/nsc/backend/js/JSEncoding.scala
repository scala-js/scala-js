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
trait JSEncoding extends SubComponent {
  import global._

  import scala.tools.jsm.ast.{ Trees => js }
  import js.{ Position => JSPosition }

  def encodeLabelSym(sym: Symbol)(implicit pos: JSPosition): js.Ident = {
    require(sym.isLabel, "encodeLabelSym called with non-label symbol")
    js.Ident("$jslabel$" + sym.name.toString + "$" + sym.id)
  }

  def encodeFieldSym(sym: Symbol)(implicit pos: JSPosition): js.PropertyName = {
    require(sym.owner.isClass && sym.isTerm && !sym.isMethod && !sym.isModule,
        "encodeFieldSym called with non-field symbol")

    js.PropertyName("$jsfield$" + sym.name.toString)
  }

  def encodeMethodSym(sym: Symbol)(implicit pos: JSPosition): js.PropertyName = {
    require(sym.isMethod, "encodeMethodSym called with non-method symbol")
    js.PropertyName(sym.name.toString + makeParamsString(sym))
  }

  def encodeLocalSym(sym: Symbol)(implicit pos: JSPosition): js.Ident = {
    require(sym.owner.isMethod && sym.isTerm && !sym.isMethod && !sym.isModule,
        "encodeLocalSym called with non-local symbol")

    if (sym.isValueParameter) js.Ident(sym.name.toString)
    else js.Ident(sym.name.toString + "$jsid$" + sym.id)
  }

  def encodeClassSym(sym: Symbol)(implicit pos: JSPosition): js.Ident = {
    require(sym.isClass, "encodeClassSym called with non-class symbol")
    js.Ident("$jsclass$" + mangleFullName(sym) + suffixFor(sym))
  }

  def encodeClassOfSym(sym: Symbol)(implicit pos: JSPosition): js.Ident = {
    require(sym.isClass, "encodeClassOfSym called with non-class symbol")
    js.Ident("$jsclassof$" + mangleFullName(sym) + suffixFor(sym))
  }

  def encodeModuleSymInternal(sym: Symbol)(implicit pos: JSPosition): js.Ident = {
    require(sym.isModule, "encodeModuleSymInternal called with non-module symbol")
    js.Ident("$jsmodulevar$" + mangleFullName(sym) + "$")
  }

  def encodeModuleSym(sym: Symbol)(implicit pos: JSPosition): js.Ident = {
    require(sym.isModule, "encodeModuleSym called with non-module symbol")
    js.Ident("$jsmodule$" + mangleFullName(sym) + "$")
  }

  private def mangleFullName(sym: Symbol) =
    sym.fullName.replaceAllLiterally(".", "$dot")

  private def suffixFor(sym: Symbol) =
    if (sym.hasModuleFlag && !sym.isMethod && !sym.isImplClass) "$" else ""

  // Encoding of method signatures

  private def makeParamsString(sym: Symbol): String = {
    sym.tpe match {
      case MethodType(params, resultType) =>
        makeParamsString(params.toList map (_.tpe), resultType)

      case NullaryMethodType(resultType) =>
        makeParamsString(Nil, resultType)

      case _ => abort("Expected a method type for " + sym)
    }
  }

  private def makeParamsString(paramTypes: List[Type], resultType: Type): String =
    makeParamsString(paramTypes map typeFullName, typeFullName(resultType))

  private def makeParamsString(paramTypeNames: List[String], resultTypeName: String) =
    paramTypeNames.mkString("(", ",", ")") + ":" + resultTypeName

  private def typeFullName(tpe: Type): String = tpe match {
    case TypeRef(_, definitions.ArrayClass, List(elementType)) =>
      typeFullName(elementType) + "[]"
    case _ => tpe.typeSymbol.fullName
  }
}
