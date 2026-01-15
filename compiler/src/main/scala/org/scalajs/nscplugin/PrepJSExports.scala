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

package org.scalajs.nscplugin

import scala.collection.mutable

import scala.tools.nsc.Global

import org.scalajs.ir.Trees.TopLevelExportDef.isValidTopLevelExportName
import org.scalajs.ir.WellKnownNames.DefaultModuleID

/** Prepare export generation
 *
 *  Helpers for transformation of @JSExport annotations
 */
trait PrepJSExports[G <: Global with Singleton] { this: PrepJSInterop[G] =>

  import global._
  import jsAddons._
  import definitions._
  import jsDefinitions._

  import scala.reflect.internal.Flags

  private sealed abstract class ExportDestination

  private object ExportDestination {

    /** Export in the "normal" way: as an instance member, or at the top-level
     *  for naturally top-level things (classes and modules).
     */
    case object Normal extends ExportDestination

    /** Export at the top-level. */
    case class TopLevel(moduleID: String) extends ExportDestination

    /** Export as a static member of the companion class. */
    case object Static extends ExportDestination
  }

  /* Not final because it causes the following compile warning:
   * "The outer reference in this type test cannot be checked at run time."
   */
  private case class ExportInfo(jsName: String,
      destination: ExportDestination)(val pos: Position)

  /** Generate exports for the given Symbol.
   *
   *  * Registers top-level and static exports.
   *  * Returns (non-static) exporters for this symbol.
   */
  def genExport(sym: Symbol): List[Tree] = {
    // Scala classes are never exported: Their constructors are.
    val isScalaClass = sym.isClass && !sym.isTrait && !sym.isModuleClass && !isJSAny(sym)

    /* Filter case class apply (and unapply) to work around
     * https://github.com/scala/bug/issues/8826
     */
    val isCaseApplyOrUnapplyParam = sym.isLocalToBlock && sym.owner.isCaseApplyOrUnapply

    /* Filter constructors of module classes: The module classes themselves will
     * be exported.
     */
    val isModuleClassCtor = sym.isConstructor && sym.owner.isModuleClass

    val exports =
      if (isScalaClass || isCaseApplyOrUnapplyParam || isModuleClassCtor) Nil
      else exportsOf(sym)

    assert(exports.isEmpty || !sym.isBridge,
        s"found exports for bridge symbol $sym. exports: $exports")

    val (normalExports, topLevelAndStaticExports) =
      exports.partition(_.destination == ExportDestination.Normal)

    /* We can handle top level exports and static exports entirely in the
     * backend. So just register them here.
     *
     * For accessors, we need to apply some special logic to static and
     * top-level exports: They actually apply to the *fields*, not to the
     * accessors.
     */
    if (sym.isAccessor && sym.accessed != NoSymbol) {
      /* Only forward registration from the getter (not the setter) to avoid
       * duplicate registration.
       */
      if (sym.isGetter)
        registerStaticAndTopLevelExports(sym.accessed, topLevelAndStaticExports)
    } else {
      registerStaticAndTopLevelExports(sym, topLevelAndStaticExports)
    }

    // For normal exports, generate exporter methods.
    normalExports.flatMap(exp => genExportDefs(sym, exp.jsName, exp.pos))
  }

  private def registerStaticAndTopLevelExports(sym: Symbol,
      exports: List[ExportInfo]): Unit = {
    val topLevel = exports.collect {
      case info @ ExportInfo(jsName, ExportDestination.TopLevel(moduleID)) =>
        jsInterop.TopLevelExportInfo(moduleID, jsName)(info.pos)
    }

    if (topLevel.nonEmpty)
      jsInterop.registerTopLevelExports(sym, topLevel)

    val static = exports.collect {
      case info @ ExportInfo(jsName, ExportDestination.Static) =>
        jsInterop.StaticExportInfo(jsName)(info.pos)
    }

    if (static.nonEmpty)
      jsInterop.registerStaticExports(sym, static)
  }

  /** retrieves the names a sym should be exported to from its annotations
   *
   *  Note that for accessor symbols, the annotations of the accessed symbol
   *  are used, rather than the annotations of the accessor itself.
   */
  private def exportsOf(sym: Symbol): List[ExportInfo] = {
    val trgSym = {
      def isOwnerScalaClass = !sym.owner.isModuleClass && !isJSAny(sym.owner)

      // For primary Scala class constructors, look on the class itself
      if (sym.isPrimaryConstructor && isOwnerScalaClass) sym.owner
      else sym
    }

    val symOwner =
      if (sym.isConstructor) sym.owner.owner
      else sym.owner

    // Annotations that are directly on the member
    val directAnnots = trgSym.annotations.filter(annot => isDirectMemberAnnot(annot.symbol))

    /* Annotations for this member on the whole unit
     *
     * Note that for top-level classes / modules this is always empty, because
     * packages cannot have annotations.
     */
    val unitAnnots = {
      val useExportAll = {
        sym.isPublic &&
        !sym.isSynthetic &&
        !sym.isConstructor &&
        !sym.isTrait &&
        (!sym.isClass || sym.isModuleClass)
      }

      if (useExportAll)
        symOwner.annotations.filter(_.symbol == JSExportAllAnnotation)
      else
        Nil
    }

    val allAnnots = {
      val allAnnots0 = directAnnots ++ unitAnnots

      if (allAnnots0.nonEmpty) {
        if (checkExportTarget(sym, allAnnots0.head.pos)) allAnnots0
        else Nil // prevent code generation from running to avoid crashes.
      } else {
        Nil
      }
    }

    val allExportInfos = for {
      annot <- allAnnots
    } yield {
      val isExportAll = annot.symbol == JSExportAllAnnotation
      val isTopLevelExport = annot.symbol == JSExportTopLevelAnnotation
      val isStaticExport = annot.symbol == JSExportStaticAnnotation
      val hasExplicitName = annot.args.nonEmpty

      assert(!isTopLevelExport || hasExplicitName,
          "Found a top-level export without an explicit name at " + annot.pos)

      val name = {
        if (hasExplicitName) {
          annot.stringArg(0).getOrElse {
            reporter.error(annot.args(0).pos,
                s"The argument to ${annot.symbol.name} must be a literal string")
            "dummy"
          }
        } else {
          val nameBase =
            (if (sym.isConstructor) sym.owner else sym).unexpandedName

          if (nme.isSetterName(nameBase) && !jsInterop.isJSSetter(sym)) {
            reporter.error(annot.pos,
                "You must set an explicit name when " +
                "exporting a non-setter with a name ending in _=")
          }

          nameBase.decoded.stripSuffix("_=")
        }
      }

      val destination = {
        if (isTopLevelExport) {
          val moduleID = if (annot.args.size == 1) {
            DefaultModuleID
          } else {
            annot.stringArg(1).getOrElse {
              reporter.error(annot.args(1).pos,
                  "moduleID must be a literal string")
              DefaultModuleID
            }
          }

          ExportDestination.TopLevel(moduleID)
        } else if (isStaticExport) {
          ExportDestination.Static
        } else {
          ExportDestination.Normal
        }
      }

      // Enforce no __ in name
      if (!isTopLevelExport && name.contains("__")) {
        // Get position for error message
        val pos = if (hasExplicitName) annot.args.head.pos else trgSym.pos

        reporter.error(pos,
            "An exported name may not contain a double underscore (`__`)")
      }

      // Destination-specific restrictions
      destination match {
        case ExportDestination.Normal =>
          if (symOwner.hasPackageFlag) {
            // Disallow @JSExport on top-level definitions.
            reporter.error(annot.pos,
                "@JSExport is forbidden on top-level objects and classes. " +
                "Use @JSExportTopLevel instead.")
          }

          // Make sure we do not override the default export of toString
          def isIllegalToString = {
            name == "toString" && sym.name != nme.toString_ &&
            sym.tpe.params.isEmpty && !jsInterop.isJSGetter(sym)
          }
          if (isIllegalToString) {
            reporter.error(annot.pos,
                "You may not export a zero-argument " +
                "method named other than 'toString' under the name 'toString'")
          }

          /* Illegal function application exports, i.e., method named 'apply'
           * without an explicit export name.
           */
          if (!hasExplicitName && sym.name == nme.apply) {
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

              reporter.error(pos,
                  "A member cannot be exported to function " +
                  "application. Add @JSExport(\"apply\") to export under the " +
                  "name apply.")
            }
          }

        case _: ExportDestination.TopLevel =>
          if (sym.isLazy) {
            reporter.error(annot.pos,
                "You may not export a lazy val to the top level")
          } else if (!sym.isAccessor && jsInterop.isJSProperty(sym)) {
            reporter.error(annot.pos,
                "You may not export a getter or a setter to the top level")
          }

          // Disallow non-static definitions.
          if (!symOwner.isStatic || !symOwner.isModuleClass) {
            reporter.error(annot.pos,
                "Only static objects may export their members to the top level")
          }

          // The top-level name must be a valid JS identifier
          if (!isValidTopLevelExportName(name)) {
            reporter.error(annot.pos,
                "The top-level export name must be a valid JavaScript " +
                "identifier name")
          }

        case ExportDestination.Static =>
          def companionIsNonNativeJSClass: Boolean = {
            val companion = symOwner.companionClass
            companion != NoSymbol &&
              !companion.isTrait &&
              isJSAny(companion) &&
              !companion.hasAnnotation(JSNativeAnnotation)
          }

          if (!symOwner.isStatic || !symOwner.isModuleClass ||
              !companionIsNonNativeJSClass) {
            reporter.error(annot.pos,
                "Only a static object whose companion class is a " +
                "non-native JS class may export its members as static.")
          }

          if (sym.isLazy) {
            reporter.error(annot.pos,
                "You may not export a lazy val as static")
          }

          // Illegal function application export
          if (!hasExplicitName && sym.name == nme.apply) {
            reporter.error(annot.pos,
                "A member cannot be exported to function application as " +
                "static. Use @JSExportStatic(\"apply\") to export it under " +
                "the name 'apply'.")
          }

          if (sym.isClass || sym.isConstructor) {
            reporter.error(annot.pos,
                "Implementation restriction: cannot export a class or " +
                "object as static")
          }
      }

      ExportInfo(name, destination)(annot.pos)
    }

    allExportInfos.filter(_.destination == ExportDestination.Normal)
      .groupBy(_.jsName)
      .filter { case (jsName, group) =>
        if (jsName == "apply" && group.size == 2)
          // @JSExportAll and single @JSExport("apply") should not be warned.
          !unitAnnots.exists(_.symbol == JSExportAllAnnotation)
        else
          group.size > 1
      }
      .foreach(_ => reporter.warning(sym.pos, s"Found duplicate @JSExport"))

    /* Check that no field is exported *twice* as static, nor both as static
     * and as top-level (it is possible to export a field several times as
     * top-level, though).
     */
    if (sym.isGetter) {
      for {
        firstStatic <- allExportInfos.find(_.destination == ExportDestination.Static).toList
        duplicate <- allExportInfos
        if duplicate ne firstStatic
      } {
        duplicate.destination match {
          case ExportDestination.Normal => // OK

          case ExportDestination.Static =>
            reporter.error(duplicate.pos,
                "Fields (val or var) cannot be exported as static more " +
                "than once")

          case _: ExportDestination.TopLevel =>
            reporter.error(duplicate.pos,
                "Fields (val or var) cannot be exported both as static " +
                "and at the top-level")
        }
      }
    }

    allExportInfos.distinct
  }

  /** Checks whether the given target is suitable for export and exporting
   *  should be performed.
   *
   *  Reports any errors for unsuitable targets.
   *  @returns a boolean indicating whether exporting should be performed. Note:
   *      a result of true is not a guarantee that no error was emitted. But it is
   *      a guarantee that the target is not "too broken" to run the rest of
   *      the generation. This approximation is done to avoid having to complicate
   *      shared code verifying conditions.
   */
  private def checkExportTarget(sym: Symbol, errPos: Position): Boolean = {
    def err(msg: String) = {
      reporter.error(errPos, msg)
      false
    }

    def hasLegalExportVisibility(sym: Symbol) =
      sym.isPublic || sym.isProtected && !sym.isProtectedLocal

    lazy val params = sym.paramss.flatten

    def hasIllegalDefaultParam = {
      val isDefParam = (_: Symbol).hasFlag(Flags.DEFAULTPARAM)
      params.reverse.dropWhile(isDefParam).exists(isDefParam)
    }

    def hasAnyNonPrivateCtor: Boolean =
      sym.info.member(nme.CONSTRUCTOR).filter(!isPrivateMaybeWithin(_)).exists

    if (sym.isTrait) {
      err("You may not export a trait")
    } else if (sym.hasAnnotation(JSNativeAnnotation)) {
      err("You may not export a native JS definition")
    } else if (!hasLegalExportVisibility(sym)) {
      err("You may only export public and protected definitions")
    } else if (sym.isConstructor && !hasLegalExportVisibility(sym.owner)) {
      err("You may only export constructors of public and protected classes")
    } else if (sym.isMacro) {
      err("You may not export a macro")
    } else if (isJSAny(sym.owner)) {
      err("You may not export a member of a subclass of js.Any")
    } else if (scalaPrimitives.isPrimitive(sym)) {
      err("You may not export a primitive")
    } else if (sym.isLocalToBlock) {
      err("You may not export a local definition")
    } else if (sym.isConstructor && sym.owner.isLocalToBlock) {
      err("You may not export constructors of local classes")
    } else if (params.nonEmpty && params.init.exists(isRepeated _)) {
      err("In an exported method or constructor, a *-parameter must come last " +
        "(through all parameter lists)")
    } else if (hasIllegalDefaultParam) {
      err("In an exported method or constructor, all parameters with " +
        "defaults must be at the end")
    } else if (sym.isConstructor && sym.owner.isAbstractClass && !isJSAny(sym)) {
      err("You may not export an abstract class")
    } else if (sym.isClass && !sym.isModuleClass && isJSAny(sym) && !hasAnyNonPrivateCtor) {
      /* This test is only relevant for JS classes: We'll complain on the
       * individual exported constructors in case of a Scala class.
       */
      err("You may not export a class that has only private constructors")
    } else {
      if (jsInterop.isJSSetter(sym))
        checkSetterSignature(sym, errPos, exported = true)

      true // ok even if a setter has the wrong signature.
    }
  }

  /** generate an exporter for a target including default parameter methods */
  private def genExportDefs(sym: Symbol, jsName: String, pos: Position) = {
    val siblingSym =
      if (sym.isConstructor) sym.owner
      else sym

    val clsSym = siblingSym.owner

    val isProperty = sym.isModuleClass || isJSAny(sym) || jsInterop.isJSProperty(sym)
    val scalaName = jsInterop.scalaExportName(jsName, isProperty)

    val copiedFlags = siblingSym.flags & (Flags.PROTECTED | Flags.FINAL)

    // Create symbol for new method
    val expSym = clsSym.newMethod(scalaName, pos, Flags.SYNTHETIC | copiedFlags)
    expSym.privateWithin = siblingSym.privateWithin

    val expSymTpe = {
      /* Alter type for new method (lift return type to Any)
       * The return type is lifted, in order to avoid bridge
       * construction and to detect methods whose signature only differs
       * in the return type.
       */
      if (sym.isClass) NullaryMethodType(AnyClass.tpe)
      else retToAny(sym.tpe.cloneInfo(expSym))
    }

    expSym.setInfoAndEnter(expSymTpe)

    // Construct exporter DefDef tree
    val exporter = genProxyDefDef(sym, expSym, pos)

    // Construct exporters for default getters
    val defaultGetters = for {
      (param, i) <- expSym.paramss.flatten.zipWithIndex
      if param.hasFlag(Flags.DEFAULTPARAM)
    } yield genExportDefaultGetter(clsSym, sym, expSym, i + 1, pos)

    exporter :: defaultGetters
  }

  private def genExportDefaultGetter(clsSym: Symbol, trgMethod: Symbol,
      exporter: Symbol, paramPos: Int, pos: Position) = {

    // Get default getter method we'll copy
    val trgGetter =
      clsSym.tpe.member(nme.defaultGetterName(trgMethod.name, paramPos))

    assert(trgGetter.exists,
        s"Cannot find default getter for param $paramPos of $trgMethod")

    // Although the following must be true in a correct program, we cannot
    // assert, since a graceful failure message is only generated later
    if (!trgGetter.isOverloaded) {
      val expGetter = trgGetter.cloneSymbol

      expGetter.name = nme.defaultGetterName(exporter.name, paramPos)
      expGetter.pos = pos

      clsSym.info.decls.enter(expGetter)

      genProxyDefDef(trgGetter, expGetter, pos)

    } else EmptyTree
  }

  /** generate a DefDef tree (from [[proxySym]]) that calls [[trgSym]] */
  private def genProxyDefDef(trgSym: Symbol, proxySym: Symbol, pos: Position) = atPos(pos) {
    val tpeParams = proxySym.typeParams.map(gen.mkAttributedIdent(_))

    // Construct proxied function call
    val nonPolyFun = {
      if (trgSym.isConstructor) {
        val clsTpe = trgSym.owner.tpe
        val tpe = gen.mkTypeApply(TypeTree(clsTpe), tpeParams)
        Select(New(tpe), trgSym)
      } else if (trgSym.isModuleClass) {
        assert(proxySym.paramss.isEmpty,
            s"got a module export with non-empty paramss. target: $trgSym, proxy: $proxySym at $pos")
        gen.mkAttributedRef(trgSym.sourceModule)
      } else if (trgSym.isClass) {
        assert(isJSAny(trgSym), s"got a class export for a non-JS class ($trgSym) at $pos")
        assert(proxySym.paramss.isEmpty,
            s"got a class export with non-empty paramss. target: $trgSym, proxy: $proxySym at $pos")
        val tpe = gen.mkTypeApply(TypeTree(trgSym.tpe), tpeParams)
        gen.mkTypeApply(gen.mkAttributedRef(JSPackage_constructorOf), List(tpe))
      } else {
        val fun = gen.mkAttributedRef(trgSym)
        gen.mkTypeApply(fun, tpeParams)
      }
    }

    val rhs = proxySym.paramss.foldLeft(nonPolyFun) { (fun, params) =>
      val args = params.map { param =>
        val ident = gen.mkAttributedIdent(param)

        if (isRepeated(param)) Typed(ident, Ident(tpnme.WILDCARD_STAR))
        else ident
      }

      Apply(fun, args)
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

  /** Whether a symbol is an annotation that goes directly on a member */
  private lazy val isDirectMemberAnnot = Set[Symbol](
    JSExportAnnotation,
    JSExportTopLevelAnnotation,
    JSExportStaticAnnotation
  )

}
