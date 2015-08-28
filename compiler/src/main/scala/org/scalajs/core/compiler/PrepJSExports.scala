/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Tobias Schlatter
 */

package org.scalajs.core.compiler

import scala.collection.mutable

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

  case class ExportInfo(
      jsName: String,
      pos: Position,
      isNamed: Boolean,
      ignoreInvalid: Boolean
  ) extends jsInterop.ExportInfo

  /** Generate the exporter for the given DefDef
   *
   *  If this DefDef is a constructor, it is registered to be exported by
   *  GenJSCode instead and no trees are returned.
   */
  def genExportMember(ddef: DefDef): List[Tree] = {
    val baseSym = ddef.symbol
    val clsSym = baseSym.owner

    val exports = exportsOf(baseSym)
    val ignoreInvalid = exports.forall(_.ignoreInvalid)

    // Helper function for errors
    def err(msg: String) = {
      if (!ignoreInvalid)
        reporter.error(exports.head.pos, msg)
      Nil
    }

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
      else if (clsSym.isAbstractClass)
        err("You may not export an abstract class")
      else if (clsSym.isLocalToBlock)
        err("You may not export a local class")
      else if (clsSym.isNestedClass)
        err(s"You may not export a nested class. $createFactoryInOuterClassHint")
      else {
        jsInterop.registerForExport(baseSym, exports)
        Nil
      }
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

  /** Checks and registers module exports on the symbol */
  def registerModuleExports(sym: Symbol): Unit = {
    assert(sym.isModuleClass, "Expected module class")
    registerClassOrModuleExportsInternal(sym)
  }

  /** Checks and registers class exports on the symbol. */
  def registerClassExports(sym: Symbol): Unit = {
    assert(!sym.isModuleClass && sym.hasAnnotation(ScalaJSDefinedAnnotation),
        "Expected a Scala.js-defined JS class")
    registerClassOrModuleExportsInternal(sym)
  }

  private def registerClassOrModuleExportsInternal(sym: Symbol): Unit = {
    val isMod = sym.isModuleClass

    val exports = exportsOf(sym)
    val ignoreInvalid = exports.forall(_.ignoreInvalid)

    if (exports.nonEmpty) {
      def err(msg: String) = {
        if (!ignoreInvalid)
          reporter.error(exports.head.pos, msg)
      }

      def hasAnyNonPrivateCtor: Boolean =
        sym.info.member(nme.CONSTRUCTOR).filter(!isPrivateMaybeWithin(_)).exists

      if (!hasLegalExportVisibility(sym)) {
        err("You may only export public and protected " +
            (if (isMod) "objects" else "classes"))
      } else if (sym.isLocalToBlock) {
        err("You may not export a local " +
            (if (isMod) "object" else "class"))
      } else if (!sym.owner.hasPackageFlag) {
        err("You may not export a nested " +
            (if (isMod) "object" else s"class. $createFactoryInOuterClassHint"))
      } else if (sym.isAbstractClass) {
        err("You may not export an abstract class")
      } else if (!isMod && !hasAnyNonPrivateCtor) {
        err("You may not export a class that has only private constructors")
      } else {
        val (named, normal) = exports.partition(_.isNamed)

        for {
          exp <- named
          if !exp.ignoreInvalid
        } {
          reporter.error(exp.pos, "You may not use @JSNamedExport on " +
              (if (isMod) "an object" else "a Scala.js-defined JS class"))
        }

        jsInterop.registerForExport(sym, normal)
      }
    }
  }

  private def createFactoryInOuterClassHint = {
    "Create an exported factory method in the outer class to work " +
    "around this limitation."
  }

  /** retrieves the names a sym should be exported to from its annotations
   *
   *  Note that for accessor symbols, the annotations of the accessed symbol
   *  are used, rather than the annotations of the accessor itself.
   */
  def exportsOf(sym: Symbol): List[ExportInfo] = {
    val exports = directExportsOf(sym) ++ inheritedExportsOf(sym)

    // Calculate the distinct exports for this symbol (eliminate double
    // occurrences of (name, isNamed) pairs).
    val grouped = exports.groupBy(exp => (exp.jsName, exp.isNamed))

    for (((jsName, isNamed), exps) <- grouped.toList)
      // Make sure that we are strict if necessary
      yield exps.find(!_.ignoreInvalid).getOrElse(exps.head)
  }

  private def directExportsOf(sym: Symbol): List[ExportInfo] = {
    val trgSym = {
      def isOwnerScalaClass = !sym.owner.isModuleClass && !isJSAny(sym.owner)

      // For accessors, look on the val/var def
      if (sym.isAccessor) sym.accessed
      // For primary Scala class constructors, look on the class itself
      else if (sym.isPrimaryConstructor && isOwnerScalaClass) sym.owner
      else sym
    }

    // Annotations that are directly on the member
    val directAnnots = for {
      annot <- trgSym.annotations
      if annot.symbol == JSExportAnnotation ||
         annot.symbol == JSExportNamedAnnotation
    } yield annot

    // Is this a member export (i.e. not a class or module export)?
    val isMember = sym.isMethod && !sym.isConstructor

    // Annotations for this member on the whole unit
    val unitAnnots = {
      if (isMember && sym.isPublic && !sym.isSynthetic)
        sym.owner.annotations.filter(_.symbol == JSExportAllAnnotation)
      else
        Nil
    }

    for {
      annot <- directAnnots ++ unitAnnots
    } yield {
      val isNamedExport = annot.symbol == JSExportNamedAnnotation
      val isExportAll = annot.symbol == JSExportAllAnnotation
      val hasExplicitName = annot.args.nonEmpty

      def explicitName = annot.stringArg(0).getOrElse {
        reporter.error(annot.pos,
            s"The argument to ${annot.symbol.name} must be a literal string")
        "dummy"
      }

      val name = {
        if (hasExplicitName) explicitName
        else if (sym.isConstructor) decodedFullName(sym.owner)
        else if (sym.isClass) decodedFullName(sym)
        else sym.unexpandedName.decoded.stripSuffix("_=")
      }

      // Enforce proper setter signature
      if (jsInterop.isJSSetter(sym))
        checkSetterSignature(sym, annot.pos, exported = true)

      // Enforce no __ in name
      if (name.contains("__")) {
        // Get position for error message
        val pos = if (hasExplicitName) annot.args.head.pos else trgSym.pos

        reporter.error(pos,
            "An exported name may not contain a double underscore (`__`)")
      }

      // Make sure we do not override the default export of toString
      def isIllegalToString = {
        isMember && !isNamedExport &&
        name == "toString" && sym.name != nme.toString_ &&
        sym.tpe.params.isEmpty && !jsInterop.isJSGetter(sym)
      }

      if (isIllegalToString) {
        reporter.error(annot.pos, "You may not export a zero-argument " +
            "method named other than 'toString' under the name 'toString'")
      }

      def isIllegalApplyExport = {
        isMember && !hasExplicitName &&
        sym.name == nme.apply &&
        !(isExportAll && directAnnots.exists(annot =>
            annot.symbol == JSExportAnnotation &&
            annot.args.nonEmpty &&
            annot.stringArg(0) == Some("apply")))
      }

      // Don't allow apply without explicit name
      if (isIllegalApplyExport) {
        // Get position for error message
        val pos = if (isExportAll) trgSym.pos else annot.pos

        reporter.warning(pos, "Member cannot be exported to function " +
            "application. It is available under the name apply instead. " +
            "Add @JSExport(\"apply\") to silence this warning. " +
            "This will be enforced in 1.0.")
      }

      if (isNamedExport && jsInterop.isJSProperty(sym)) {
        reporter.error(annot.pos,
            "You may not export a getter or a setter as a named export")
      }

      ExportInfo(name, annot.pos, isNamedExport, ignoreInvalid = false)
    }
  }

  private def inheritedExportsOf(sym: Symbol): List[ExportInfo] = {
    // The symbol from which we (potentially) inherit exports. It also
    // gives the exports their name
    val trgSym = {
      if (sym.isModuleClass || (sym.isClass && isJSAny(sym))) {
        sym
      } else if (sym.isConstructor && sym.isPublic && !isJSAny(sym.owner) &&
          sym.owner.isConcreteClass && !sym.owner.isModuleClass) {
        sym.owner
      } else {
        NoSymbol
      }
    }

    if (trgSym == NoSymbol) {
      Nil
    } else {
      val trgAnnot =
        if (sym.isModuleClass) JSExportDescendentObjectsAnnotation
        else JSExportDescendentClassesAnnotation

      val forcingSymInfos = for {
        forcingSym <- trgSym.ancestors
        annot      <- forcingSym.annotations
        if annot.symbol == trgAnnot
      } yield {
        val ignoreInvalid = annot.constantAtIndex(0).fold(false)(_.booleanValue)
        (forcingSym, ignoreInvalid)
      }

      // The dominating forcing symbol, is the first that does not ignore
      // or the first otherwise
      val forcingSymInfo =
        forcingSymInfos.find(!_._2).orElse(forcingSymInfos.headOption)

      val name = decodedFullName(trgSym)
      val nameValid = !name.contains("__")

      val optExport = for {
        (forcingSym, ignoreInvalid) <- forcingSymInfo
        if nameValid || !ignoreInvalid
      } yield {
        // Enfore no __ in name
        if (!nameValid) {
          // Get all annotation positions for error message
          reporter.error(sym.pos,
              s"${trgSym.name} may not have a double underscore (`__`) in " +
              "its fully qualified name, since it is forced to be exported by " +
              s"a @${trgAnnot.name} on $forcingSym")
        }

        ExportInfo(name, sym.pos, false, ignoreInvalid)
      }

      optExport.toList
    }
  }

  /** Just like sym.fullName, but does not encode components */
  private def decodedFullName(sym: Symbol): String = {
    if (sym.isRoot || sym.isRootPackage || sym == NoSymbol) sym.name.decoded
    else if (sym.owner.isEffectiveRoot) sym.name.decoded
    else decodedFullName(sym.effectiveOwner.enclClass) + '.' + sym.name.decoded
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

  /** Whether the given symbol has a visibility that allows exporting */
  private def hasLegalExportVisibility(sym: Symbol): Boolean =
    sym.isPublic || sym.isProtected && !sym.isProtectedLocal

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
