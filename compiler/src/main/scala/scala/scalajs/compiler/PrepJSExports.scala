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
    else if (isJSAny(baseSym.owner))
      err(s"You may not export a $memType of a subclass of js.Any")
    else if (!baseSym.isPublic)
      err(s"You may not export a non-public $memType")
    else if (baseSym.isMacro)
      err("You may not export a macro")
    else if (scalaPrimitives.isPrimitive(baseSym))
      err("You may not export a primitive")
    else if (!hasAllowedRetType(baseSym.tpe)) {
      err("""You may not export a method whose return type is neither a subtype of
            |AnyRef nor a concrete subtype of AnyVal (i.e. a value class or a
            |primitive value type).""".stripMargin)
    } else if (forScaladoc) {
      /* Don't do anything under scaladoc because the uncurry phase does not
       * exist in that setting (see bug #323). It's no big deal because we do
       * not need exports for scaladoc
       */
      Nil
    } else if (baseSym.isConstructor) {
      // we can generate constructors entirely in the backend, since they
      // do not need inheritance and such. But we want to check their sanity
      // here
      Nil
    } else {
      assert(!baseSym.isBridge)

      // Reset interface flag: Any trait will contain non-empty methods
      clsSym.resetFlag(Flags.INTERFACE)

      // Actually generate exporter methods
      for ((jsName, pos) <- exportNames)
        yield atPos(pos) { genExportDef(baseSym, jsName, pos) }
    }
  }

  /** generate an exporter for a DefDef */
  private def genExportDef(defSym: Symbol, jsName: String, pos: Position) = {
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

    def spliceParam(sym: Symbol) = {
      if (isRepeated(sym))
        Typed(Ident(sym), Ident(tpnme.WILDCARD_STAR))
      else
        Ident(sym)
    }

    // Construct inner function call
    val sel: Tree = Select(This(clsSym), defSym)
    val rhs = (sel /: expSym.paramss) {
      (fun,params) => Apply(fun, params map spliceParam)
    }

    // Construct and type the actual tree
    typer.typedDefDef(DefDef(expSym, rhs))
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

}
