/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Tobias Schlatter
 */

package org.scalajs.core.compiler

import scala.annotation.tailrec

import scala.tools.nsc.NoPhase

/**
 *  Prepare export generation
 *
 *  Helpers for transformation of @JSExport annotations
 */
trait PrepJSExports { this: PrepJSInterop =>

  import global._
  import jsAddons._
  import definitions._
  import jsDefinitions._

  import scala.reflect.internal.Flags

  /** Whether the given symbol has a visibility that allows exporting */
  def hasLegalExportVisibility(sym: Symbol): Boolean =
    sym.isPublic || sym.isProtected && !sym.isProtectedLocal

  def genExportMember(ddef: DefDef): List[Tree] = {
    val baseSym = ddef.symbol
    val clsSym = baseSym.owner

    val exports = jsInterop.exportsOf(baseSym)

    // Helper function for errors
    def err(msg: String) = { reporter.error(exports.head.pos, msg); Nil }
    def memType = if (baseSym.isConstructor) "constructor" else "method"

    if (exports.isEmpty)
      Nil
    else if (!hasLegalExportVisibility(baseSym))
      err(s"You may only export public and protected ${memType}s")
    else if (baseSym.isMacro)
      err("You may not export a macro")
    else if (scalaPrimitives.isPrimitive(baseSym))
      err("You may not export a primitive")
    else if (hasIllegalRepeatedParam(baseSym))
      err(s"In an exported $memType, a *-parameter must come last " +
        "(through all parameter lists)")
    else if (hasIllegalDefaultParam(baseSym))
      err(s"In an exported $memType, all parameters with defaults " +
        "must be at the end")
    else if (baseSym.isConstructor) {
      // we can generate constructors entirely in the backend, since they
      // do not need inheritance and such. But we want to check their sanity
      // here by previous tests and the following ones.

      if (!hasLegalExportVisibility(clsSym))
        err("You may only export public and protected classes")
      else if (clsSym.isLocalToBlock)
        err("You may not export a local class")
      else if (clsSym.isNestedClass)
        err("You may not export a nested class. Create an exported factory " +
            "method in the outer class to work around this limitation.")
      else Nil

    } else {
      assert(!baseSym.isBridge)

      // Reset interface flag: Any trait will contain non-empty methods
      clsSym.resetFlag(Flags.INTERFACE)

      // Actually generate exporter methods
      exports.flatMap { exp =>
        if (exp.isNamed)
          genNamedExport(baseSym, exp.jsName, exp.pos) :: Nil
        else
          genExportDefs(baseSym, exp.jsName, exp.pos)
      }
    }
  }

  /** generate an exporter for a DefDef including default parameter methods */
  private def genExportDefs(defSym: Symbol, jsName: String, pos: Position) = {
    val clsSym = defSym.owner
    val scalaName =
      jsInterop.scalaExportName(jsName, jsInterop.isJSProperty(defSym))

    // Create symbol for new method
    val expSym = defSym.cloneSymbol

    // Set position of symbol
    expSym.pos = pos

    // Alter type for new method (lift return type to Any)
    // The return type is lifted, in order to avoid bridge
    // construction and to detect methods whose signature only differs
    // in the return type.
    // Attention: This will cause boxes for primitive value types and value
    // classes. However, since we have restricted the return types, we can
    // always safely remove these boxes again in the back-end.
    if (!defSym.isConstructor)
      expSym.setInfo(retToAny(expSym.tpe))

    // Change name for new method
    expSym.name = scalaName

    // Update flags
    expSym.setFlag(Flags.SYNTHETIC)
    expSym.resetFlag(
        Flags.DEFERRED     | // We always have a body
        Flags.ACCESSOR     | // We are never a "direct" accessor
        Flags.CASEACCESSOR | // And a fortiori not a case accessor
        Flags.LAZY         | // We are not a lazy val (even if we export one)
        Flags.OVERRIDE       // Synthetic methods need not bother with this
    )

    // Remove export annotations
    expSym.removeAnnotation(JSExportAnnotation)
    expSym.removeAnnotation(JSExportNamedAnnotation)

    // Add symbol to class
    clsSym.info.decls.enter(expSym)

    // Construct exporter DefDef tree
    val exporter = genProxyDefDef(clsSym, defSym, expSym, pos)

    // Construct exporters for default getters
    val defaultGetters = for {
      (param, i) <- expSym.paramss.flatten.zipWithIndex
      if param.hasFlag(Flags.DEFAULTPARAM)
    } yield genExportDefaultGetter(clsSym, defSym, expSym, i + 1, pos)

    exporter :: defaultGetters
  }

  /** Generate a dummy DefDef tree for a named export. This tree is captured
   *  by GenJSCode again to generate the required JavaScript logic.
   */
  private def genNamedExport(defSym: Symbol, jsName: String, pos: Position) = {
    val clsSym = defSym.owner
    val scalaName = jsInterop.scalaExportName(jsName, false)

    // Create symbol for the new exporter method
    val expSym = clsSym.newMethodSymbol(scalaName, pos,
        Flags.SYNTHETIC | Flags.FINAL)

    // Mark the symbol to be a named export
    expSym.addAnnotation(JSExportNamedAnnotation)

    // Create a single parameter of type Any
    val param = expSym.newValueParameter(newTermName("namedArgs"), pos)
    param.setInfo(AnyTpe)

    // Set method type
    expSym.setInfo(MethodType(param :: Nil, AnyClass.tpe))

    // Register method to parent
    clsSym.info.decls.enter(expSym)

    // Placeholder tree
    def ph = Ident(Predef_???)

    // Create a call to the forwarded method with ??? as args
    val sel: Tree = Select(This(clsSym), defSym)
    val call = (sel /: defSym.paramss) {
      (fun, params) => Apply(fun, List.fill(params.size)(ph))
    }

    // rhs is a block to prevent boxing of result
    typer.typedDefDef(DefDef(expSym, Block(call, ph)))
  }

  private def genExportDefaultGetter(clsSym: Symbol, trgMethod: Symbol,
      exporter: Symbol, paramPos: Int, pos: Position) = {

    // Get default getter method we'll copy
    val trgGetter =
      clsSym.tpe.member(nme.defaultGetterName(trgMethod.name, paramPos))

    assert(trgGetter.exists)

    // Although the following must be true in a correct program, we cannot
    // assert, since a graceful failure message is only generated later
    if (!trgGetter.isOverloaded) {
      val expGetter = trgGetter.cloneSymbol

      expGetter.name = nme.defaultGetterName(exporter.name, paramPos)
      expGetter.pos  = pos

      clsSym.info.decls.enter(expGetter)

      genProxyDefDef(clsSym, trgGetter, expGetter, pos)

    } else EmptyTree
  }

  /** generate a DefDef tree (from [[proxySym]]) that calls [[trgSym]] */
  private def genProxyDefDef(clsSym: Symbol, trgSym: Symbol,
      proxySym: Symbol, pos: Position) = atPos(pos) {

    // Helper to ascribe repeated argument lists when calling
    def spliceParam(sym: Symbol) = {
      if (isRepeated(sym))
        Typed(Ident(sym), Ident(tpnme.WILDCARD_STAR))
      else
        Ident(sym)
    }

    // Construct proxied function call
    val sel: Tree = Select(This(clsSym), trgSym)
    val rhs = (sel /: proxySym.paramss) {
      (fun,params) => Apply(fun, params map spliceParam)
    }

    typer.typedDefDef(DefDef(proxySym, rhs))
  }

  /** changes the return type of the method type tpe to Any. returns new type */
  private def retToAny(tpe: Type): Type = tpe match {
    case MethodType(params, result) => MethodType(params, retToAny(result))
    case NullaryMethodType(result)  => NullaryMethodType(AnyClass.tpe)
    case PolyType(tparams, result)  => PolyType(tparams, retToAny(result))
    case _                          => AnyClass.tpe
  }

  /** checks whether this type has a repeated parameter elsewhere than at the end
    * of all the params
    */
  private def hasIllegalRepeatedParam(sym: Symbol): Boolean = {
    val params = sym.paramss.flatten
    params.nonEmpty && params.init.exists(isRepeated _)
  }

  /** checks whether there are default parameters not at the end of
    * the flattened parameter list
    */
  private def hasIllegalDefaultParam(sym: Symbol): Boolean = {
    val isDefParam = (_: Symbol).hasFlag(Flags.DEFAULTPARAM)
    sym.paramss.flatten.reverse.dropWhile(isDefParam).exists(isDefParam)
  }

}
