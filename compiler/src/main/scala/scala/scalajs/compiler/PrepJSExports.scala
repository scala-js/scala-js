/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Tobias Schlatter
 */

package scala.scalajs.compiler

import scala.annotation.tailrec

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

  def genExportMember(ddef: DefDef): List[Tree] = {
    val baseSym = ddef.symbol
    val clsSym = baseSym.owner

    val exportNames = jsInterop.exportsOf(baseSym)

    // Helper function for errors
    def err(msg: String) = { currentUnit.error(exportNames.head._2, msg); Nil }
    def memType = if (baseSym.isConstructor) "constructor" else "method"

    if (exportNames.isEmpty)
      Nil
    else if (isJSAny(clsSym))
      err(s"You may not export a $memType of a subclass of js.Any")
    else if (!baseSym.isPublic)
      err(s"You may not export a non-public $memType")
    else if (baseSym.isMacro)
      err("You may not export a macro")
    else if (scalaPrimitives.isPrimitive(baseSym))
      err("You may not export a primitive")
    else if (!hasAllowedRetType(baseSym.tpe))
      err("""You may not export a method whose return type is neither a subtype of
            |AnyRef nor a concrete subtype of AnyVal (i.e. a value class or a
            |primitive value type).""".stripMargin)
    else if (hasIllegalRepeatedParam(baseSym))
      err(s"In an exported $memType, a *-parameter must come last " +
        "(through all parameter lists)")
    else if (hasIllegalDefaultParam(baseSym))
      err(s"In an exported $memType, all parameters with defaults " +
        "must be at the end")
    else if (forScaladoc) {
      /* Don't do anything under scaladoc because the uncurry phase does not
       * exist in that setting (see bug #323). It's no big deal because we do
       * not need exports for scaladoc
       */
      Nil
    } else if (baseSym.isConstructor) {
      // we can generate constructors entirely in the backend, since they
      // do not need inheritance and such. But we want to check their sanity
      // here by previous tests and the following ones.

      if (!clsSym.isPublic)
        err("You may not export a non-public class")
      else if (clsSym.isLocal)
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
      for {
        (jsName, pos) <- exportNames
        tree          <- genExportDefs(baseSym, jsName, pos)
      } yield tree
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
        Flags.DEFERRED |  // We always have a body now
        Flags.OVERRIDE    // Synthetic methods need not bother with this
    )

    // Remove JSExport annotations
    expSym.removeAnnotation(JSExportAnnotation)

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
    case _: TypeRef                 => AnyClass.tpe
    case _ => abort(s"Type of method is not method type, but ${tpe}")
  }

  /** checks whether the type is subtype of AnyRef (or generic with bounds),
   *  a primitive value type, or a value class */
  @tailrec
  private def hasAllowedRetType(tpe: Type): Boolean = {
    val sym = tpe.typeSymbol // this may be NoSymbol

    (tpe <:< AnyRefClass.tpe) ||
    sym.isPrimitiveValueClass ||
    sym.isDerivedValueClass || {
      tpe match {
        case MethodType(_, retTpe)     => hasAllowedRetType(retTpe)
        case NullaryMethodType(retTpe) => hasAllowedRetType(retTpe)
        // Note that in the PolyType case, the return type may be polymorphic,
        // but the conformance test correctly works with bounds.
        // Therefore, `T <: AnyRef` is a valid return type.
        case PolyType(_, retTpe)       => hasAllowedRetType(retTpe)
        case _ => false
      }
    }
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
