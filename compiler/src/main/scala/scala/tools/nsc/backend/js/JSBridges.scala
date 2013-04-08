/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package js

import scala.reflect.internal.Flags

/** Generation of bridges for JavaScript
 *
 *  @author Sébastien Doeraene
 */
trait JSBridges extends SubComponent { self: GenJSCode =>
  val global: scalajs.JSGlobal

  import global._

  private def isCandidateForBridge(sym: Symbol): Boolean =
    sym.isMethod && !sym.isConstructor && !sym.isBridge && sym.isPublic

  def genBridgesForClass(sym: Symbol): List[js.Tree] = {
    val declaredMethods = sym.info.decls.filter(isCandidateForBridge)
    val newlyDeclaredMethods =
      declaredMethods.filterNot(_.isOverridingSymbol)
    val newlyDeclaredMethodNames =
      newlyDeclaredMethods.map(_.name).toList.distinct
    newlyDeclaredMethodNames map (genBridge(sym, _))
  }

  private def genBridge(classSym: Symbol, name: TermName): js.Tree = {
    val overloaded = classSym.info.member(name)
    val alts = overloaded.alternatives.filter(isCandidateForBridge)
    assert(!alts.isEmpty)

    implicit val pos = alts.head.pos

    val altsByArgCount = alts.groupBy(_.tpe.params.size).toList.sortBy(_._1)

    val maxArgCount = altsByArgCount.last._1
    val formalsArgs = genFormalArgs(maxArgCount)

    val cases = for {
      (argc, methods) <- altsByArgCount
    } yield {
      (js.IntLiteral(argc), genBridgeSameArgc(methods))
    }

    val body = {
      if (cases.size == 1) cases.head._2
      else {
        js.Switch(js.DotSelect(js.Ident("arguments"), js.Ident("length")),
            cases, genThrowTypeError(alts))
      }
    }

    js.MethodDef(js.PropertyName(name.toString()), formalsArgs, body)
  }

  private def genBridgeSameArgc(alts: List[Symbol]): js.Tree = {
    // TODO Discriminate by runtime type tests
    genTieBreak(alts)
  }

  private def genTieBreak(alts: List[Symbol]): js.Tree = {
    // TODO For now we just emit all calls (the first one wins)
    implicit val pos = alts.head.pos
    js.Block(alts map genApplyForSym)
  }

  private def genApplyForSym(sym: Symbol): js.Tree = {
    implicit val pos = sym.pos
    js.Return {
      js.ApplyMethod(js.This(), encodeMethodSym(sym),
          genFormalArgs(sym.tpe.params.size))
    }
  }

  private def genThrowTypeError(alts: List[Symbol])(
      implicit pos: Position): js.Tree = {
    js.Throw(js.StringLiteral("No matching overload"))
  }

  private def genFormalArgs(count: Int)(implicit pos: Position): List[js.Ident] =
    (1 to count map genFormalArg).toList

  private def genFormalArg(index: Int)(implicit pos: Position): js.Ident =
    js.Ident("arg$" + index)
}
