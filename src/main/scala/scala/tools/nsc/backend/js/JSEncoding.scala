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
  import scala.tools.jsm.Names
  import Names.{ IdentifierName => JSIdentName, PropertyName => JSPropName }

  private def strForSymbol(sym: Symbol): String = {
    val nameString = sym.name.toString

    val name = if (sym.name.isTypeName)
      "$jstype$" + sym.fullName
    else if (sym.isModule)
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
    makeIdent(strForSymbol(sym))
  }

  def propForSymbol(sym: Symbol)(implicit pos: JSPosition): js.PropIdent = {
    makeProp(strForSymbol(sym))
  }

  def varForClass(sym: Symbol)(implicit pos: JSPosition): js.Ident = {
    makeIdent("$jsclass$" + sym.fullName + suffixFor(sym))
  }

  def varForModuleInternal(sym: Symbol)(implicit pos: JSPosition): js.Ident = {
    makeIdent("$jsmodulevar$" + sym.fullName + "$")
  }

  def varForModule(sym: Symbol)(implicit pos: JSPosition): js.Ident = {
    makeIdent("$jsmodule$" + sym.fullName + "$")
  }

  private def suffixFor(sym: Symbol) =
    if (sym.hasModuleFlag && !sym.isMethod && !sym.isImplClass) "$" else ""

  private def makeIdent(name: String)(implicit pos: JSPosition): js.Ident =
    js.Ident(new JSIdentName(name))

  private def makeProp(name: String)(implicit pos: JSPosition): js.PropIdent =
    js.PropIdent(new JSPropName(name))
}
