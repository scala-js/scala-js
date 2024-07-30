/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.linker.backend.emitter

import org.scalajs.ir.Position

import org.scalajs.linker.backend.javascript.Trees._
import org.scalajs.linker.interface.ESVersion

/** Collection of tree generators that are used across the board.
 *  This class is fully stateless.
 *
 *  Also carries around config (semantics and esFeatures).
 */
private[emitter] final class JSGen(val config: Emitter.Config) {

  import config._
  import coreSpec._

  /** Should we use ECMAScript classes for JavaScript classes and Throwable
   *  classes?
   *
   *  This is true iff `useECMAScript2015Semantics` is true, independently of
   *  [[org.scalajs.linker.interface.ESFeatures.avoidClasses ESFeatures.avoidClasses]].
   *
   *  We must emit classes for JavaScript classes for semantics reasons:
   *  inheritance of static properties and ability to extend native JavaScript
   *  ES classes.
   *
   *  We must emit classes Throwable classes so that they are recognized as
   *  proper JavaScript error classes, which gives them better support in
   *  debuggers.
   */
  val useClassesForJSClassesAndThrowables = esFeatures.useECMAScript2015Semantics

  /** Should we use ECMAScript classes for non-Throwable Scala classes?
   *
   *  If [[org.scalajs.linker.interface.ESFeatures.avoidClasses ESFeatures.avoidClasses]]
   *  is true, we do not use classes for non-Throwable classes. We can do that
   *  because whether regular classes are compiled as classes or functions and
   *  prototypes has no impact on observable semantics.
   *
   *  `useClassesForRegularClasses` is always false when
   *  `useClassesForJSClassesAndThrowables` is false.
   */
  val useClassesForRegularClasses =
    useClassesForJSClassesAndThrowables && !esFeatures.avoidClasses

  /** Should we emit `let`s and `const`s for all internal variables?
   *
   *  See [[org.scalajs.linker.interface.ESFeatures.avoidLetsAndConsts ESFeatures.avoidLetsAndConsts]]
   *  for a rationale.
   *
   *  Note: top-level exports in Script (`NoModule`) mode are always
   *  emitted as `let`s under ECMAScript 2015 semantics, irrespective of this
   *  value.
   */
  val useLets = esFeatures.esVersion >= ESVersion.ES2015 && !esFeatures.avoidLetsAndConsts

  def genConst(name: Ident, rhs: Tree)(implicit pos: Position): LocalDef =
    genLet(name, mutable = false, rhs)

  def genLet(name: Ident, mutable: Boolean, rhs: Tree)(
      implicit pos: Position): LocalDef = {
    if (useLets)
      Let(name, mutable, Some(rhs))
    else
      VarDef(name, Some(rhs))
  }

  def genEmptyMutableLet(name: Ident)(implicit pos: Position): LocalDef =
    genEmptyLet(name, mutable = true)

  def genEmptyImmutableLet(name: Ident)(implicit pos: Position): LocalDef =
    genEmptyLet(name, mutable = false)

  private def genEmptyLet(name: Ident, mutable: Boolean)(
      implicit pos: Position): LocalDef = {
    if (useLets)
      Let(name, mutable, rhs = None)
    else
      VarDef(name, rhs = None)
  }

  def genBracketSelect(qual: Tree, item: Tree)(implicit pos: Position): Tree = {
    item match {
      case StringLiteral(name) if optimizeBracketSelects &&
          Ident.isValidJSIdentifierName(name) && name != "eval" =>
        /* We exclude "eval" because we do not want to rely too much on the
         * strict mode peculiarities of eval(), so that we can keep running
         * on VMs that do not support strict mode.
         */
        DotSelect(qual, Ident(name))
      case _ =>
        BracketSelect(qual, item)
    }
  }

  def genIdentBracketSelect(qual: Tree, item: String)(
      implicit pos: Position): Tree = {
    require(item != "eval")
    if (optimizeBracketSelects)
      DotSelect(qual, Ident(item))
    else
      BracketSelect(qual, StringLiteral(item))
  }

  /** Generates an arrow function if supported by the ES version.
   *
   *  This is independent of the ECMAScript 2015 *semantics*. This method must
   *  not be used for closures that are *specified* to be arrow functions in
   *  ES 2015 but `function`s in ES 5.1 semantics. In other words, it must not
   *  be used to compile `ir.Trees.Closure`s.
   */
  def genArrowFunction(args: List[ParamDef], restParam: Option[ParamDef], body: Tree)(
      implicit pos: Position): Function = {
    Function(esFeatures.esVersion >= ESVersion.ES2015, args, restParam, body)
  }

  def genDefineProperty(obj: Tree, prop: Tree, descriptor: List[(String, Tree)])(
      implicit tracking: GlobalRefTracking, pos: Position): WithGlobals[Tree] = {
    val descriptorTree =
        ObjectConstr(descriptor.map(x => StringLiteral(x._1) -> x._2))

    globalRef("Object").map { objRef =>
      Apply(genIdentBracketSelect(objRef, "defineProperty"),
          List(obj, prop, descriptorTree))
    }
  }

  def globalRef(name: String)(
      implicit tracking: GlobalRefTracking, pos: Position): WithGlobals[VarRef] = {
    val trackedSet: Set[String] =
      if (tracking.shouldTrack(name)) Set(name)
      else Set.empty
    WithGlobals(VarRef(Ident(name)), trackedSet)
  }

  def genPropSelect(qual: Tree, item: PropertyName)(
      implicit pos: Position): Tree = {
    item match {
      case item: MaybeDelayedIdent => DotSelect(qual, item)
      case item: StringLiteral     => genBracketSelect(qual, item)
      case ComputedName(tree)      => genBracketSelect(qual, tree)
    }
  }

  def genIIFE(captures: List[(ParamDef, Tree)], body: Tree)(
      implicit pos: Position): Tree = {
    val (params, args) = captures.unzip
    Apply(genArrowFunction(params, None, body), args)
  }
}
