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

  private def strForSymbol(sym: Symbol): String = {
    require(sym.isTerm, "strForSymbol requires a term symbol")

    val nameString = sym.name.toString

    val name = if (sym.isModule)
      throw new AssertionError("varForSymbol for module requested" + sym)
    else if (sym.isLabel)
      "$jslabel$" + nameString + "$" + sym.id
    else if (nameString endsWith " ")
      "$jsendspace$" + nameString.substring(0, nameString.length-1)
    else if (sym.isMethod)
      nameString
    else if (sym.owner.isClass /*&& !(nameString endsWith " ")*/)
      "$jsfield$" + nameString
    else if (sym.owner.isMethod && (!sym.isValueParameter))
      nameString + "$jsid$" + sym.id
    else
      nameString

    name + suffixFor(sym)
  }

  def varForSymbol(sym: Symbol)(implicit pos: JSPosition): js.Ident = {
    js.Ident(strForSymbol(sym))
  }

  def propForSymbol(sym: Symbol)(implicit pos: JSPosition): js.PropertyName = {
    js.PropertyName(strForSymbol(sym))
  }

  def varForType(sym: Symbol)(implicit pos: JSPosition): js.Ident = {
    js.Ident("$jstype$" + mangleFullName(sym) + suffixFor(sym))
  }

  def varForClass(sym: Symbol)(implicit pos: JSPosition): js.Ident = {
    js.Ident("$jsclass$" + mangleFullName(sym) + suffixFor(sym))
  }

  def varForModuleInternal(sym: Symbol)(implicit pos: JSPosition): js.Ident = {
    js.Ident("$jsmodulevar$" + mangleFullName(sym) + "$")
  }

  def varForModule(sym: Symbol)(implicit pos: JSPosition): js.Ident = {
    js.Ident("$jsmodule$" + mangleFullName(sym) + "$")
  }

  private def mangleFullName(sym: Symbol) =
    sym.fullName.replaceAllLiterally(".", "$dot")

  private def suffixFor(sym: Symbol) =
    if (sym.hasModuleFlag && !sym.isMethod && !sym.isImplClass) "$" else ""
}
