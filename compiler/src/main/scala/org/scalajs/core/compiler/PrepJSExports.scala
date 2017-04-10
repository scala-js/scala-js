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

  case class ExportInfo(jsName: String,
      destination: ExportDestination)(val pos: Position)
      extends jsInterop.ExportInfo

  private final val SuppressExportDeprecationsMsg = {
    "\n  (you can suppress this warning in 0.6.x by passing the option " +
    "`-P:scalajs:suppressExportDeprecations` to scalac)"
  }

  /** Generate the exporter for the given DefDef
   *  or ValDef (abstract val in class, val in trait or lazy val;
   *  these don't get DefDefs until the fields phase)
   *
   *  If this DefDef is a constructor, it is registered to be exported by
   *  GenJSCode instead and no trees are returned.
   */
  def genExportMember(baseSym: Symbol): List[Tree] = {
    val clsSym = baseSym.owner

    val exports = exportsOf(baseSym)

    // Helper function for errors
    def err(msg: String) = {
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
      else if (!clsSym.isStatic)
        err(s"You may not export a nested class. $createFactoryInOuterClassHint")
      else {
        jsInterop.registerForExport(baseSym, exports)
        Nil
      }
    } else {
      assert(!baseSym.isBridge)

      // Reset interface flag: Any trait will contain non-empty methods
      clsSym.resetFlag(Flags.INTERFACE)

      val (normalExports, topLevelAndStaticExports) =
        exports.partition(_.destination == ExportDestination.Normal)

      /* We can handle top level exports and static exports entirely in the
       * backend. So just register them here.
       */
      jsInterop.registerForExport(baseSym, topLevelAndStaticExports)

      // Actually generate exporter methods
      normalExports.flatMap(exp => genExportDefs(baseSym, exp.jsName, exp.pos))
    }
  }

  /** Checks and registers module exports on the symbol */
  def registerModuleExports(sym: Symbol): Unit = {
    assert(sym.isModuleClass, "Expected module class")
    registerClassOrModuleExportsInternal(sym)
  }

  /** Checks and registers class exports on the symbol. */
  def registerClassExports(sym: Symbol): Unit = {
    assert(!sym.isModuleClass && !sym.hasAnnotation(JSNativeAnnotation),
        "Expected a Scala.js-defined JS class")
    registerClassOrModuleExportsInternal(sym)
  }

  private def registerClassOrModuleExportsInternal(sym: Symbol): Unit = {
    val isMod = sym.isModuleClass

    val exports = exportsOf(sym)

    if (exports.nonEmpty) {
      def err(msg: String) = {
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
      } else if (!sym.isStatic) {
        err("You may not export a nested " +
            (if (isMod) "object" else s"class. $createFactoryInOuterClassHint"))
      } else if (sym.isAbstractClass) {
        err("You may not export an abstract class")
      } else if (!isMod && !hasAnyNonPrivateCtor) {
        err("You may not export a class that has only private constructors")
      } else {
        jsInterop.registerForExport(sym, exports)
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
    val trgSym = {
      def isOwnerScalaClass = !sym.owner.isModuleClass && !isJSAny(sym.owner)

      /* For accessors, look on the val/var def, if there is one.
       * TODO Get rid of this when we can break binary compatibility, as
       * @JSExport and @JSExportNamed are now annotated with
       * @field @getter @setter
       */
      if (sym.isAccessor && sym.accessed != NoSymbol) sym.accessed
      // For primary Scala class constructors, look on the class itself
      else if (sym.isPrimaryConstructor && isOwnerScalaClass) sym.owner
      else sym
    }

    // Annotations that are directly on the member
    val directAnnots = trgSym.annotations.filter(
        annot => isDirectMemberAnnot(annot.symbol))

    // Is this a member export (i.e. not a class or module export)?
    val isMember = sym.isMethod && !sym.isConstructor

    // Annotations for this member on the whole unit
    val unitAnnots = {
      if (isMember && sym.isPublic && !sym.isSynthetic)
        sym.owner.annotations.filter(_.symbol == JSExportAllAnnotation)
      else
        Nil
    }

    val allExportInfos = for {
      annot <- directAnnots ++ unitAnnots
    } yield {
      val isExportAll = annot.symbol == JSExportAllAnnotation
      val isTopLevelExport = annot.symbol == JSExportTopLevelAnnotation
      val isStaticExport = annot.symbol == JSExportStaticAnnotation
      val hasExplicitName = annot.args.nonEmpty

      assert(!isTopLevelExport || hasExplicitName,
          "Found a top-level export without an explicit name at " + annot.pos)

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

      val destination = {
        if (isTopLevelExport) ExportDestination.TopLevel
        else if (isStaticExport) ExportDestination.Static
        else ExportDestination.Normal
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

      /* Illegal function application exports, i.e., method named 'apply'
       * without an explicit export name.
       */
      if (isMember && !hasExplicitName && sym.name == nme.apply) {
        destination match {
          case ExportDestination.Normal =>
            def shouldBeTolerated = {
              isExportAll && directAnnots.exists { annot =>
                annot.symbol == JSExportAnnotation &&
                annot.args.nonEmpty &&
                annot.stringArg(0) == Some("apply")
              }
            }

            // Don't allow apply without explicit name
            if (!shouldBeTolerated) {
              // Get position for error message
              val pos = if (isExportAll) trgSym.pos else annot.pos

              reporter.warning(pos, "Member cannot be exported to function " +
                  "application. It is available under the name apply " +
                  "instead. Add @JSExport(\"apply\") to silence this " +
                  "warning. This will be enforced in 1.0.")
            }

          case ExportDestination.TopLevel =>
            throw new AssertionError(
                "Found a top-level export without an explicit name at " +
                annot.pos)

          case ExportDestination.Static =>
            reporter.error(annot.pos,
                "A member cannot be exported to function application as " +
                "static. Use @JSExportStatic(\"apply\") to export it under " +
                "the name 'apply'.")
        }
      }

      // Destination-specific restrictions
      destination match {
        case ExportDestination.Normal =>
          // Make sure we do not override the default export of toString
          def isIllegalToString = {
            isMember && name == "toString" && sym.name != nme.toString_ &&
            sym.tpe.params.isEmpty && !jsInterop.isJSGetter(sym)
          }
          if (isIllegalToString) {
            reporter.error(annot.pos, "You may not export a zero-argument " +
                "method named other than 'toString' under the name 'toString'")
          }

          // Don't allow nested class / module exports without explicit name.
          def isStaticNested = {
            /* For Scala.js defined JS classes, sym is the class itself. For
             * normal classes, sym is the constructor that is to be exported.
             */
            val clsSym = if (sym.isClass) sym else sym.owner
            clsSym.isNestedClass && clsSym.isStatic && !clsSym.isLocalToBlock
          }
          if (!isMember && !hasExplicitName && isStaticNested) {
            reporter.error(annot.pos,
                "You must set an explicit name for exports of nested classes.")
          }

          // Deprecate @JSExport on classes and objects
          if (!isMember && !scalaJSOpts.suppressExportDeprecations) {
            if (sym.isModuleClass) {
              reporter.warning(annot.pos,
                  "@JSExport on objects is deprecated and will be removed " +
                  "in 1.0.0. Use @JSExportTopLevel instead. Note that it " +
                  "exports the object itself (rather than a 0-arg function " +
                  "returning the object), so the calling JavaScript code " +
                  "must be adapted." +
                  SuppressExportDeprecationsMsg)
            } else {
              reporter.warning(annot.pos,
                  "@JSExport on classes is deprecated and will be removed " +
                  "in 1.0.0. Use @JSExportTopLevel instead (which does " +
                  "exactly the same thing on classes)." +
                  SuppressExportDeprecationsMsg)
            }
          }

        case ExportDestination.TopLevel =>
          if (sym.isLazy) {
            reporter.error(annot.pos,
                "You may not export a lazy val to the top level")
          } else if (!sym.isAccessor && jsInterop.isJSProperty(sym)) {
            reporter.error(annot.pos,
                "You may not export a getter or a setter to the top level")
          }

          val symOwner =
            if (sym.isConstructor) sym.owner.owner
            else sym.owner
          if (!symOwner.isStatic || !symOwner.isModuleClass) {
            reporter.error(annot.pos,
                "Only static objects may export their members to the top level")
          }

        case ExportDestination.Static =>
          val symOwner =
            if (sym.isClassConstructor) sym.owner.owner
            else sym.owner

          def companionIsScalaJSDefinedJSClass: Boolean = {
            val companion = symOwner.companionClass
            companion != NoSymbol &&
            !companion.isTrait &&
            isJSAny(companion) &&
            !companion.hasAnnotation(JSNativeAnnotation)
          }

          if (!symOwner.isStatic || !symOwner.isModuleClass ||
              !companionIsScalaJSDefinedJSClass) {
            reporter.error(annot.pos,
                "Only a static object whose companion class is a " +
                "Scala.js-defined JS class may export its members as static.")
          }

          if (isMember) {
            if (sym.isLazy) {
              reporter.error(annot.pos,
                  "You may not export a lazy val as static")
            }
          } else {
            if (sym.isTrait) {
              reporter.error(annot.pos,
                  "You may not export a trait as static.")
            } else {
              reporter.error(annot.pos,
                  "Implementation restriction: cannot export a class or " +
                  "object as static")
            }
          }
      }

      ExportInfo(name, destination)(annot.pos)
    }

    /* Filter out static exports of accessors (as they are not actually
     * exported, their fields are). The above is only used to uniformly perform
     * checks.
     */
    val filteredExports = if (!sym.isAccessor || sym.accessed == NoSymbol) {
      allExportInfos
    } else {
      /* For accessors, we need to apply some special logic to static exports.
       * When tested on accessors, they actually apply on *fields*, not on the
       * accessors. We use the same code paths hereabove to uniformly perform
       * relevant checks, but at the end of the day, we have to throw away the
       * ExportInfo.
       * However, we must make sure that no field is exported *twice* as static,
       * nor both as static and as top-level (it is possible to export a field
       * several times as top-level, though).
       */
      val (topLevelAndStaticExportInfos, actualExportInfos) =
        allExportInfos.partition(_.destination != ExportDestination.Normal)

      if (sym.isGetter) {
        topLevelAndStaticExportInfos.find {
          _.destination == ExportDestination.Static
        }.foreach { firstStatic =>
          for {
            duplicate <- topLevelAndStaticExportInfos
            if duplicate ne firstStatic
          } {
            if (duplicate.destination == ExportDestination.Static) {
              reporter.error(duplicate.pos,
                  "Fields (val or var) cannot be exported as static more " +
                  "than once")
            } else {
              reporter.error(duplicate.pos,
                  "Fields (val or var) cannot be exported both as static " +
                  "and at the top-level")
            }
          }
        }

        jsInterop.registerForExport(sym.accessed, topLevelAndStaticExportInfos)
      }

      actualExportInfos
    }

    filteredExports.distinct
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

  /** Whether a symbol is an annotation that goes directly on a member */
  private lazy val isDirectMemberAnnot = Set[Symbol](
      JSExportAnnotation,
      JSExportTopLevelAnnotation,
      JSExportStaticAnnotation
  )

}
