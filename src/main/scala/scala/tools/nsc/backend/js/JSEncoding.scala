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

    // TODO encode signature
    js.PropertyName(sym.name.toString)
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
}
