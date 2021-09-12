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

import scala.tools.nsc
import nsc._

import scala.collection.immutable.ListMap
import scala.collection.mutable

import org.scalajs.ir.Trees.{JSGlobalRef, JSNativeLoadSpec}

/** Prepares classes extending js.Any for JavaScript interop
 *
 * This phase does:
 * - Sanity checks for js.Any hierarchy
 * - Annotate subclasses of js.Any to be treated specially
 * - Rewrite calls to scala.Enumeration.Value (include name string)
 * - Create JSExport methods: Dummy methods that are propagated
 *   through the whole compiler chain to mark exports. This allows
 *   exports to have the same semantics than methods.
 *
 * @author Tobias Schlatter
 */
abstract class PrepJSInterop[G <: Global with Singleton](val global: G)
    extends plugins.PluginComponent with PrepJSExports[G]
    with transform.Transform with CompatComponent {

  import PrepJSInterop._

  /** Not for use in the constructor body: only initialized afterwards. */
  val jsAddons: JSGlobalAddons {
    val global: PrepJSInterop.this.global.type
  }

  /** Not for use in the constructor body: only initialized afterwards. */
  val scalaJSOpts: ScalaJSOptions

  import global._
  import jsAddons._
  import definitions._
  import rootMirror._
  import jsDefinitions._
  import jsInterop.{JSCallingConvention, JSName}

  val phaseName: String = "jsinterop"
  override def description: String = "prepare ASTs for JavaScript interop"

  override def newPhase(p: nsc.Phase): StdPhase = new JSInteropPhase(p)

  class JSInteropPhase(prev: nsc.Phase) extends Phase(prev) {
    override def name: String = phaseName
    override def description: String = PrepJSInterop.this.description
    override def run(): Unit = {
      jsPrimitives.initPrepJSPrimitives()
      jsInterop.clearGlobalState()
      super.run()
    }
  }

  override protected def newTransformer(unit: CompilationUnit): Transformer =
    new JSInteropTransformer(unit)

  private object jsnme {
    val hasNext  = newTermName("hasNext")
    val next     = newTermName("next")
    val nextName = newTermName("nextName")
    val Value    = newTermName("Value")
    val Val      = newTermName("Val")

    val ArrowAssoc = newTermName("ArrowAssoc")
  }

  class JSInteropTransformer(unit: CompilationUnit) extends Transformer {

    /** Kind of the directly enclosing (most nested) owner. */
    private var enclosingOwner: OwnerKind = OwnerKind.None

    /** Cumulative kinds of all enclosing owners. */
    private var allEnclosingOwners: OwnerKind = OwnerKind.None

    /** Nicer syntax for `allEnclosingOwners is kind`. */
    private def anyEnclosingOwner: OwnerKind = allEnclosingOwners

    /** Nicer syntax for `allEnclosingOwners isnt kind`. */
    private object noEnclosingOwner {
      @inline def is(kind: OwnerKind): Boolean =
        allEnclosingOwners isnt kind
    }

    private def enterOwner[A](kind: OwnerKind)(body: => A): A = {
      require(kind.isBaseKind, kind)
      val oldEnclosingOwner = enclosingOwner
      val oldAllEnclosingOwners = allEnclosingOwners
      enclosingOwner = kind
      allEnclosingOwners |= kind
      try {
        body
      } finally {
        enclosingOwner = oldEnclosingOwner
        allEnclosingOwners = oldAllEnclosingOwners
      }
    }

    /** Tests whether this is a ScalaDoc run.
     *
     *  There are some things we must not do in ScalaDoc runs because, because
     *  ScalaDoc runs don't do everything we need, for example constant-folding
     *  'final val's.
     *
     *  At the same time, it's no big deal to skip these things, because we
     *  won't reach the backend.
     *
     *  We don't completely disable this phase under ScalaDoc mostly because
     *  we want to keep the addition of `JSType` annotations, so that they
     *  appear in the doc.
     *
     *  Preparing exports, however, is a pure waste of time, which we cannot
     *  do properly anyway because of the aforementioned limitation.
     */
    private def forScaladoc = global.isInstanceOf[doc.ScaladocGlobal]

    /** Whether to check that we have proper literals in some crucial places. */
    private def shouldCheckLiterals = !forScaladoc

    /** Whether to check and prepare exports. */
    private def shouldPrepareExports = !forScaladoc

    /** DefDefs in class templates that export methods to JavaScript */
    private val exporters = mutable.Map.empty[Symbol, mutable.ListBuffer[Tree]]

    override def transform(tree: Tree): Tree = {
      tree match {
        case tree: MemberDef => transformMemberDef(tree)
        case tree: Template  => transformTemplateTree(tree)
        case _               => transformStatOrExpr(tree)
      }
    }

    private def transformMemberDef(tree: MemberDef): Tree = {
      val sym = moduleToModuleClass(tree.symbol)

      checkInternalAnnotations(sym)

      /* Checks related to @js.native:
       * - if @js.native, verify that it is allowed in this context, and if
       *   yes, compute and store the JS native load spec
       * - if not @js.native, verify that we do not use any other annotation
       *   reserved for @js.native members (namely, JS native load spec annots)
       */
      val isJSNative = sym.hasAnnotation(JSNativeAnnotation)
      if (isJSNative)
        checkJSNativeDefinition(tree.pos, sym)
      else
        checkJSNativeSpecificAnnotsOnNonJSNative(tree)

      checkJSNameAnnots(sym)
      checkDuplicateJSMemberAnnots(sym)

      // @unchecked needed because MemberDef is not marked `sealed`
      val transformedTree: Tree = (tree: @unchecked) match {
        case tree: ImplDef =>
          if (shouldPrepareExports)
            registerClassOrModuleExports(sym)

          if ((enclosingOwner is OwnerKind.JSNonNative) && sym.owner.isTrait && !sym.isTrait) {
            reporter.error(tree.pos,
                "Non-native JS traits cannot contain inner classes or objects")
          }

          if (isJSAny(sym))
            transformJSImplDef(tree)
          else
            transformScalaImplDef(tree)

        case tree: ValOrDefDef =>
          /* Prepare exports for methods, local defs and local variables.
           * Avoid *fields* (non-local non-method) because they all have a
           * corresponding getter, which is the one that handles exports.
           * (Note that local-to-block can never have exports, but the error
           * messages for that are handled by genExportMember).
           */
          if (shouldPrepareExports && (sym.isMethod || sym.isLocalToBlock)) {
            exporters.getOrElseUpdate(sym.owner, mutable.ListBuffer.empty) ++=
              genExportMember(sym)
          }

          if (sym.isLocalToBlock) {
            super.transform(tree)
          } else if (isJSNative) {
            transformJSNativeValOrDefDef(tree)
          } else if (enclosingOwner is OwnerKind.JSType) {
            val fixedTree = tree match {
              case tree: DefDef => fixPublicBeforeTyper(tree)
              case _            => tree
            }
            transformValOrDefDefInJSType(fixedTree)
          } else {
            transformScalaValOrDefDef(tree)
          }

        case _:TypeDef | _:PackageDef =>
          super.transform(tree)
      }

      /* Give tree.symbol, not sym, so that for modules it is the module
       * symbol, not the module class symbol.
       *
       * #1899 This must be done *after* transforming the member def tree,
       * because fixPublicBeforeTyper must have run.
       */
      markExposedIfRequired(tree.symbol)

      transformedTree
    }

    private def transformScalaImplDef(tree: ImplDef): Tree = {
      val sym = moduleToModuleClass(tree.symbol)
      val isModuleDef = tree.isInstanceOf[ModuleDef]

      // In native JS things, only js.Any stuff is allowed
      if (enclosingOwner is OwnerKind.JSNative) {
        /* We have to allow synthetic companion objects here, as they get
         * generated when a nested native JS class has default arguments in
         * its constructor (see #1891).
         */
        if (!sym.isSynthetic) {
          reporter.error(tree.pos,
              "Native JS traits, classes and objects cannot contain inner " +
              "Scala traits, classes or objects (i.e., not extending js.Any)")
        }
      }

      if (sym == UnionClass)
        sym.addAnnotation(JSTypeAnnot)

      val kind = if (sym.isSubClass(ScalaEnumClass)) {
        if (isModuleDef) OwnerKind.EnumMod
        else if (sym == ScalaEnumClass) OwnerKind.EnumImpl
        else OwnerKind.EnumClass
      } else {
        if (isModuleDef) OwnerKind.NonEnumScalaMod
        else OwnerKind.NonEnumScalaClass
      }
      enterOwner(kind) {
        super.transform(tree)
      }
    }

    private def transformScalaValOrDefDef(tree: ValOrDefDef): Tree = {
      tree match {
        // Catch ValDefs in enumerations with simple calls to Value
        case ValDef(mods, name, tpt, ScalaEnumValue.NoName(optPar))
            if anyEnclosingOwner is OwnerKind.Enum =>
          val nrhs = ScalaEnumValName(tree.symbol.owner, tree.symbol, optPar)
          treeCopy.ValDef(tree, mods, name, transform(tpt), nrhs)

        // Exporter generation
        case _ =>
          super.transform(tree)
      }
    }

    private def transformTemplateTree(tree: Template): Template = {
      val Template(parents, self, body) = tree

      /* Do not transform `self`. We do not need to perform any checks on
       * it (#3998).
       */
      val transformedParents = parents.map(transform(_))
      val nonTransformedSelf = self
      val transformedBody = body.map(transform(_))

      val clsSym = tree.symbol.owner

      // Check that @JSExportStatic fields come first
      if (clsSym.isModuleClass) { // quick check to avoid useless work
        var foundStatOrNonStaticVal: Boolean = false
        for (tree <- transformedBody) {
          tree match {
            case vd: ValDef if vd.symbol.hasAnnotation(JSExportStaticAnnotation) =>
              if (foundStatOrNonStaticVal) {
                reporter.error(vd.pos,
                    "@JSExportStatic vals and vars must be defined before " +
                    "any other val/var, and before any constructor " +
                    "statement.")
              }
            case vd: ValDef if !vd.symbol.isLazy =>
              foundStatOrNonStaticVal = true
            case _: MemberDef =>
            case _ =>
              foundStatOrNonStaticVal = true
          }
        }
      }

      // Add exports to the template, if there are any
      val transformedBodyWithExports = exporters.get(clsSym).fold {
        transformedBody
      } { exports =>
        transformedBody ::: exports.toList
      }

      treeCopy.Template(tree, transformedParents, nonTransformedSelf,
          transformedBodyWithExports)
    }

    private def transformStatOrExpr(tree: Tree): Tree = {
      tree match {
        /* Anonymous function, need to check that it is not used as a SAM for a
         * JS type, unless it is a JS function type.
         * See #2921.
         */
        case tree: Function =>
          // tpeSym is the type of the target SAM type (not the to-be-generated anonymous class)
          val tpeSym = tree.tpe.typeSymbol
          if (isJSAny(tpeSym)) {
            def reportError(reasonAndExplanation: String): Unit = {
              reporter.error(tree.pos,
                  "Using an anonymous function as a SAM for the JavaScript " +
                  s"type ${tpeSym.fullNameString} is not allowed because " +
                  reasonAndExplanation)
            }
            if (!tpeSym.isTrait || tpeSym.superClass != JSFunctionClass) {
              reportError(
                  "it is not a trait extending js.Function. " +
                  "Use an anonymous class instead.")
            } else if (tpeSym.hasAnnotation(JSNativeAnnotation)) {
              reportError(
                  "it is a native JS type. " +
                  "It is not possible to directly implement it.")
            } else if (!JSCallingConvention.isCall(samOf(tree.tpe))) {
              reportError(
                  "its single abstract method is not named `apply`. " +
                  "Use an anonymous class instead.")
            }
          }
          super.transform(tree)

        // Catch Select on Enumeration.Value we couldn't transform but need to
        // we ignore the implementation of scala.Enumeration itself
        case ScalaEnumValue.NoName(_) if noEnclosingOwner is OwnerKind.EnumImpl =>
          reporter.warning(tree.pos,
              """Couldn't transform call to Enumeration.Value.
                |The resulting program is unlikely to function properly as this
                |operation requires reflection.""".stripMargin)
          super.transform(tree)

        case ScalaEnumValue.NullName() if noEnclosingOwner is OwnerKind.EnumImpl =>
          reporter.warning(tree.pos,
              """Passing null as name to Enumeration.Value
                |requires reflection at runtime. The resulting
                |program is unlikely to function properly.""".stripMargin)
          super.transform(tree)

        case ScalaEnumVal.NoName(_) if noEnclosingOwner is OwnerKind.EnumImpl =>
          reporter.warning(tree.pos,
              """Calls to the non-string constructors of Enumeration.Val
                |require reflection at runtime. The resulting
                |program is unlikely to function properly.""".stripMargin)
          super.transform(tree)

        case ScalaEnumVal.NullName() if noEnclosingOwner is OwnerKind.EnumImpl =>
          reporter.warning(tree.pos,
              """Passing null as name to a constructor of Enumeration.Val
                |requires reflection at runtime. The resulting
                |program is unlikely to function properly.""".stripMargin)
          super.transform(tree)

        case tree if tree.symbol == ExecutionContext_global ||
            tree.symbol == ExecutionContextImplicits_global =>
          if (scalaJSOpts.warnGlobalExecutionContext) {
            global.runReporting.warning(tree.pos,
                """The global execution context in Scala.js is based on JS Promises (microtasks).
                  |Using it may prevent macrotasks (I/O, timers, UI rendering) from running reliably.
                  |
                  |Unfortunately, there is no way with ECMAScript only to implement a performant
                  |macrotask execution context (and hence Scala.js core does not contain one).
                  |
                  |We recommend you use: https://github.com/scala-js/scala-js-macrotask-executor
                  |Please refer to the README.md of that project for more details regarding
                  |microtask vs. macrotask execution contexts.
                  |
                  |If you do not care about macrotask fairness, you can silence this warning by:
                  |- Adding @nowarn("cat=other") (Scala >= 2.13.x only)
                  |- Setting the -P:scalajs:nowarnGlobalExecutionContext compiler option
                  |- Using scala.scalajs.concurrent.JSExecutionContext.queue
                  |  (the implementation of ExecutionContext.global in Scala.js) directly.
                  |
                  |If you do not care about performance, you can use
                  |scala.scalajs.concurrent.QueueExecutionContext.timeouts().
                  |It is based on setTimeout which makes it fair but slow (due to clamping).
                """.stripMargin,
                WarningCategory.Other, tree.symbol)
          }
          super.transform(tree)

        // Validate js.constructorOf[T]
        case TypeApply(ctorOfTree, List(tpeArg))
            if ctorOfTree.symbol == JSPackage_constructorOf =>
          validateJSConstructorOf(tree, tpeArg)
          super.transform(tree)

        /* Rewrite js.ConstructorTag.materialize[T] into
         * runtime.newConstructorTag[T](js.constructorOf[T])
         */
        case TypeApply(ctorOfTree, List(tpeArg))
            if ctorOfTree.symbol == JSConstructorTag_materialize =>
          validateJSConstructorOf(tree, tpeArg)
          typer.typed {
            atPos(tree.pos) {
              val ctorOf = gen.mkTypeApply(
                  gen.mkAttributedRef(JSPackage_constructorOf), List(tpeArg))
              gen.mkMethodCall(Runtime_newConstructorTag,
                  List(tpeArg.tpe), List(ctorOf))
            }
          }

        /* Rewrite js.dynamicImport[T](body) into
         *
         * runtime.dynamicImport[T](
         *   new DynamicImportThunk { def apply(): Any = body }
         * )
         */
        case Apply(TypeApply(fun, List(tpeArg)), List(body))
            if fun.symbol == JSPackage_dynamicImport =>
          val pos = tree.pos

          assert(currentOwner.isTerm, s"unexpected owner: $currentOwner at $pos")

          val clsSym = currentOwner.newClass(tpnme.ANON_CLASS_NAME, pos)
          clsSym.setInfo( // do not enter the symbol, owner is a term.
              ClassInfoType(List(DynamicImportThunkClass.tpe), newScope, clsSym))

          val ctorSym = clsSym.newClassConstructor(pos)
          ctorSym.setInfoAndEnter(MethodType(Nil, clsSym.tpe))

          val applySym = clsSym.newMethod(nme.apply)
          applySym.setInfoAndEnter(MethodType(Nil, AnyTpe))

          body.changeOwner(currentOwner -> applySym)
          val newBody = atOwner(applySym)(transform(body))

          typer.typed {
            atPos(tree.pos) {
              val superCtorCall = gen.mkMethodCall(
                  Super(clsSym, tpnme.EMPTY),
                  ObjectClass.primaryConstructor, Nil, Nil)

              // class $anon extends DynamicImportThunk
              val clsDef = ClassDef(clsSym, List(
                  // def <init>() = { super.<init>(); () }
                  DefDef(ctorSym,
                      // `gen.mkUnitBlock(gen.mkSuperInitCall)` would be better but that fails on 2.11.
                      Block(superCtorCall, Literal(Constant(())))),
                  // def apply(): Any = body
                  DefDef(applySym, newBody)))

              /* runtime.DynamicImport[A]({
               *   class $anon ...
               *   new $anon
               * })
               */
              Apply(TypeApply(gen.mkAttributedRef(Runtime_dynamicImport),
                  List(tpeArg)), List(Block(clsDef, New(clsSym))))
            }
          }

        /* Catch calls to Predef.classOf[T]. These should NEVER reach this phase
         * but unfortunately do. In normal cases, the typer phase replaces these
         * calls by a literal constant of the given type. However, when we compile
         * the scala library itself and Predef.scala is in the sources, this does
         * not happen.
         *
         * The trees reach this phase under the form:
         *
         *   scala.this.Predef.classOf[T]
         *
         * or, as of Scala 2.12.0, as:
         *
         *   scala.Predef.classOf[T]
         *
         * or so it seems, at least.
         *
         * If we encounter such a tree, depending on the plugin options, we fail
         * here or silently fix those calls.
         */
        case TypeApply(classOfTree @ Select(predef, nme.classOf), List(tpeArg))
            if predef.symbol == PredefModule =>
          if (scalaJSOpts.fixClassOf) {
            // Replace call by literal constant containing type
            if (typer.checkClassOrModuleType(tpeArg)) {
              typer.typed { Literal(Constant(tpeArg.tpe.dealias.widen)) }
            } else {
              reporter.error(tpeArg.pos, s"Type ${tpeArg} is not a class type")
              EmptyTree
            }
          } else {
            reporter.error(classOfTree.pos,
                """This classOf resulted in an unresolved classOf in the jscode
                  |phase. This is most likely a bug in the Scala compiler. ScalaJS
                  |is probably able to work around this bug. Enable the workaround
                  |by passing the fixClassOf option to the plugin.""".stripMargin)
            EmptyTree
          }

        // Compile-time errors and warnings for js.Dynamic.literal
        case Apply(Apply(fun, nameArgs), args)
            if fun.symbol == JSDynamicLiteral_applyDynamic ||
              fun.symbol == JSDynamicLiteral_applyDynamicNamed =>
          // Check that the first argument list is a constant string "apply"
          nameArgs match {
            case List(Literal(Constant(s: String))) =>
              if (s != "apply") {
                reporter.error(tree.pos,
                    s"js.Dynamic.literal does not have a method named $s")
              }
            case _ =>
              reporter.error(tree.pos,
                  s"js.Dynamic.literal.${tree.symbol.name} may not be " +
                  "called directly")
          }

          // Warn for known duplicate property names
          val knownPropNames = mutable.Set.empty[String]
          for (arg <- args) {
            def processPropName(propNameTree: Tree): Unit = {
              propNameTree match {
                case Literal(Constant(propName: String)) =>
                  if (!knownPropNames.add(propName)) {
                    reporter.warning(propNameTree.pos,
                        s"""Duplicate property "$propName" shadows a """ +
                        "previously defined one")
                  }
                case _ =>
                  // ignore
              }
            }
            arg match {
              case Apply(fun, List(propNameTree, _))
                  if fun.symbol == Tuple2_apply =>
                processPropName(propNameTree)
              case Apply(fun @ TypeApply(Select(receiver, nme.MINGT), _), _)
                  if currentRun.runDefinitions.isArrowAssoc(fun.symbol) =>
                receiver match {
                  case Apply(TypeApply(Select(predef, jsnme.ArrowAssoc), _),
                      List(propNameTree))
                      if predef.symbol == PredefModule =>
                    processPropName(propNameTree)
                  case _ =>
                    // ignore
                }
              case _ =>
                // ignore
            }
          }

          super.transform(tree)

        case _ => super.transform(tree)
      }
    }

    private def validateJSConstructorOf(tree: Tree, tpeArg: Tree): Unit = {
      val classValue = try {
        typer.typedClassOf(tree, tpeArg)
      } catch {
        case typeError: TypeError =>
          reporter.error(typeError.pos, typeError.msg)
          EmptyTree
      }

      if (classValue != EmptyTree) {
        val Literal(classConstant) = classValue
        val tpe = classConstant.typeValue.dealiasWiden
        val typeSym = tpe.typeSymbol
        if (typeSym.isTrait || typeSym.isModuleClass) {
          reporter.error(tpeArg.pos,
              s"non-trait class type required but $tpe found")
        }
      }
    }

    /** Performs checks and rewrites specific to classes / objects extending
     *  js.Any.
     */
    private def transformJSImplDef(implDef: ImplDef): Tree = {
      val sym = moduleToModuleClass(implDef.symbol)

      sym.addAnnotation(JSTypeAnnot)

      val isJSNative = sym.hasAnnotation(JSNativeAnnotation)

      val isJSFunctionSAMInScala211 =
        isScala211 && sym.name == tpnme.ANON_FUN_NAME && sym.superClass == JSFunctionClass

      // Forbid @EnableReflectiveInstantiation on JS types
      sym.getAnnotation(EnableReflectiveInstantiationAnnotation).foreach {
        annot =>
          reporter.error(annot.pos,
              "@EnableReflectiveInstantiation cannot be used on types " +
              "extending js.Any.")
      }

      // Forbid package objects that extends js.Any
      if (sym.isPackageObjectClass)
        reporter.error(implDef.pos, "Package objects may not extend js.Any.")

      // Check that we do not have a case modifier
      if (implDef.mods.hasFlag(Flag.CASE)) {
        reporter.error(implDef.pos, "Classes and objects extending " +
            "js.Any may not have a case modifier")
      }

      // Check the parents
      for (parent <- sym.info.parents) {
        parent.typeSymbol match {
          case AnyRefClass | ObjectClass =>
            // AnyRef is valid, except for non-native JS traits
            if (!isJSNative && !sym.isTrait) {
              reporter.error(implDef.pos,
                  "Non-native JS classes and objects cannot directly extend " +
                  "AnyRef. They must extend a JS class (native or not).")
            }
          case parentSym if isJSAny(parentSym) =>
            // A non-native JS type cannot extend a native JS trait
            // Otherwise, extending a JS type is valid
            if (!isJSNative && parentSym.isTrait &&
                parentSym.hasAnnotation(JSNativeAnnotation)) {
              reporter.error(implDef.pos,
                  "Non-native JS types cannot directly extend native JS " +
                  "traits.")
            }
          case DynamicClass =>
            /* We have to allow scala.Dynamic to be able to define js.Dynamic
             * and similar constructs.
             * This causes the unsoundness filed as #1385.
             */
            ()
          case SerializableClass if isJSFunctionSAMInScala211 =>
            /* Ignore the scala.Serializable trait that Scala 2.11 adds on all
             * SAM classes when on a JS function SAM.
             */
            ()
          case parentSym =>
            /* This is a Scala class or trait other than AnyRef and Dynamic,
             * which is never valid.
             */
            reporter.error(implDef.pos,
                s"${sym.nameString} extends ${parentSym.fullName} " +
                "which does not extend js.Any.")
        }
      }

      // Require that the SAM of a JS function def be `apply` (2.11-only here)
      if (isJSFunctionSAMInScala211) {
        if (!sym.info.decl(nme.apply).filter(JSCallingConvention.isCall(_)).exists) {
          val samType = sym.parentSymbols.find(_ != JSFunctionClass).getOrElse {
            /* This shouldn't happen, but fall back on this symbol (which has a
             * compiler-generated name) not to crash.
             */
            sym
          }
          reporter.error(implDef.pos,
              "Using an anonymous function as a SAM for the JavaScript type " +
              s"${samType.fullNameString} is not allowed because its single " +
              "abstract method is not named `apply`. " +
              "Use an anonymous class instead.")
        }
      }

      // Disallow bracket access / bracket call
      if (jsInterop.isJSBracketAccess(sym)) {
        reporter.error(implDef.pos,
            "@JSBracketAccess is not allowed on JS classes and objects")
      } else if (jsInterop.isJSBracketCall(sym)) {
        reporter.error(implDef.pos,
            "@JSBracketCall is not allowed on JS classes and objects")
      }

      // Checks for non-native JS stuff
      if (!isJSNative) {
        // It cannot be in a native JS class or trait
        if (enclosingOwner is OwnerKind.JSNativeClass) {
          reporter.error(implDef.pos,
              "Native JS classes and traits cannot contain non-native JS " +
              "classes, traits or objects")
        }

        // Unless it is a trait, it cannot be in a native JS object
        if (!sym.isTrait && (enclosingOwner is OwnerKind.JSNativeMod)) {
          reporter.error(implDef.pos,
              "Native JS objects cannot contain inner non-native JS " +
              "classes or objects")
        }

        // Local JS classes cannot be abstract (implementation restriction)
        if (!sym.isTrait && sym.isAbstractClass && sym.isLocalToBlock) {
          reporter.error(implDef.pos,
              "Implementation restriction: local JS classes cannot be abstract")
        }
      }

      // Check for consistency of JS semantics across overriding
      for (overridingPair <- new overridingPairs.Cursor(sym).iterator) {
        val low = overridingPair.low
        val high = overridingPair.high

        if (low.isType || high.isType) {
          /* #4375 Do nothing if either is a type, and let refchecks take care
           * of it.
           * The case where one is a type and the other is not should never
           * happen, because they would live in different namespaces and
           * therefore not override each other. However, if that should still
           * happen for some reason, rechecks should take care of it as well.
           */
        } else {
          def errorPos = {
            if (sym == low.owner) low.pos
            else if (sym == high.owner) high.pos
            else sym.pos
          }

          def memberDefString(membSym: Symbol): String =
            membSym.defStringSeenAs(sym.thisType.memberType(membSym))

          // Check for overrides with different JS names - issue #1983
          if (jsInterop.JSCallingConvention.of(low) != jsInterop.JSCallingConvention.of(high)) {
            val msg = {
              def memberDefStringWithCallingConvention(membSym: Symbol) = {
                memberDefString(membSym) +
                membSym.locationString + " called from JS as " +
                JSCallingConvention.of(membSym).displayName
              }
              "A member of a JS class is overriding another member with a different JS calling convention.\n\n" +
              memberDefStringWithCallingConvention(low) + "\n" +
              "    is conflicting with\n" +
              memberDefStringWithCallingConvention(high) + "\n"
            }

            reporter.error(errorPos, msg)
          }

          /* Cannot override a non-@JSOptional with an @JSOptional. Unfortunately
           * at this point the symbols do not have @JSOptional yet, so we need
           * to detect whether it would be applied.
           */
          if (!isJSNative) {
            def isJSOptional(sym: Symbol): Boolean = {
              sym.owner.isTrait && !sym.isDeferred && !sym.isConstructor &&
              !sym.owner.hasAnnotation(JSNativeAnnotation)
            }

            if (isJSOptional(low) && !(high.isDeferred || isJSOptional(high))) {
              reporter.error(errorPos,
                  s"Cannot override concrete ${memberDefString(high)} from " +
                  s"${high.owner.fullName} in a non-native JS trait.")
            }
          }
        }
      }

      val kind = {
        if (!isJSNative) {
          if (sym.isModuleClass) OwnerKind.JSMod
          else OwnerKind.JSClass
        } else {
          if (sym.isModuleClass) OwnerKind.JSNativeMod
          else OwnerKind.JSNativeClass
        }
      }
      enterOwner(kind) {
        super.transform(implDef)
      }
    }

    private def checkJSNativeDefinition(pos: Position, sym: Symbol): Unit = {
      // Check if we may have a JS native here
      if (sym.isLocalToBlock) {
        reporter.error(pos,
            "@js.native is not allowed on local definitions")
      } else if (!sym.isClass && (anyEnclosingOwner is (OwnerKind.ScalaClass | OwnerKind.JSType))) {
        reporter.error(pos,
            "@js.native vals and defs can only appear in static Scala objects")
      } else if (sym.isClass && !isJSAny(sym)) {
        reporter.error(pos,
            "Classes, traits and objects not extending js.Any may not have " +
            "an @js.native annotation")
      } else if (anyEnclosingOwner is OwnerKind.ScalaClass) {
        reporter.error(pos,
            "Scala traits and classes may not have native JS members")
      } else if (enclosingOwner is OwnerKind.JSNonNative) {
        reporter.error(pos,
            "non-native JS classes, traits and objects may not have " +
            "native JS members")
      } else if (!sym.isTrait) {
        /* Compute the loading spec now, before `flatten` destroys the name.
         * We store it in a global map.
         */
        val optLoadSpec = checkAndComputeJSNativeLoadSpecOf(pos, sym)
        for (loadSpec <- optLoadSpec)
          jsInterop.storeJSNativeLoadSpec(sym, loadSpec)
      } else {
        assert(sym.isTrait, sym) // just tested in the previous `if`
        for (annot <- sym.annotations) {
          val annotSym = annot.symbol
          if (JSNativeLoadingSpecAnnots.contains(annotSym)) {
            reporter.error(annot.pos,
                s"Traits may not have an @${annotSym.nameString} annotation.")
          }
        }
      }
    }

    private def checkAndComputeJSNativeLoadSpecOf(pos: Position,
        sym: Symbol): Option[JSNativeLoadSpec] = {
      import JSNativeLoadSpec._

      def makeGlobalRefNativeLoadSpec(globalRef: String,
          path: List[String]): Global = {
        val validatedGlobalRef = if (!JSGlobalRef.isValidJSGlobalRefName(globalRef)) {
          reporter.error(pos,
              "The name of a JS global variable must be a valid JS " +
              s"identifier (got '$globalRef')")
          "erroneous"
        } else {
          globalRef
        }
        Global(validatedGlobalRef, path)
      }

      if (enclosingOwner is OwnerKind.JSNative) {
        /* We cannot get here for @js.native vals and defs. That would mean we
         * have an @js.native val/def inside a JavaScript type, which is not
         * allowed and already caught in checkJSNativeDefinition().
         */
        assert(sym.isClass,
            s"undetected @js.native val or def ${sym.fullName} inside JS type at $pos")

        for (annot <- sym.annotations) {
          val annotSym = annot.symbol

          if (JSNativeLoadingSpecAnnots.contains(annotSym)) {
            reporter.error(annot.pos,
                "Nested JS classes and objects cannot " +
                s"have an @${annotSym.nameString} annotation.")
          }
        }

        if (sym.owner.isStaticOwner) {
          for (annot <- sym.annotations) {
            if (annot.symbol == JSNameAnnotation &&
                annot.args.head.tpe.typeSymbol != StringClass) {
              reporter.error(annot.pos,
                  "Implementation restriction: @JSName with a js.Symbol is " +
                  "not supported on nested native classes and objects")
            }
          }

          val jsName = jsInterop.jsNameOf(sym) match {
            case JSName.Literal(jsName) => jsName
            case JSName.Computed(_)     => "<erroneous>" // compile error above
          }

          val ownerLoadSpec = jsInterop.jsNativeLoadSpecOfOption(sym.owner)
          val loadSpec = ownerLoadSpec match {
            case None =>
              // The owner is a JSGlobalScope
              makeGlobalRefNativeLoadSpec(jsName, Nil)
            case Some(Global(globalRef, path)) =>
              Global(globalRef, path :+ jsName)
            case Some(Import(module, path)) =>
              Import(module, path :+ jsName)
            case Some(ImportWithGlobalFallback(
                Import(module, modulePath), Global(globalRef, globalPath))) =>
              ImportWithGlobalFallback(
                  Import(module, modulePath :+ jsName),
                  Global(globalRef, globalPath :+ jsName))
          }
          Some(loadSpec)
        } else {
          None
        }
      } else {
        def parsePath(pathName: String): List[String] =
          pathName.split('.').toList

        def parseGlobalPath(pathName: String): Global = {
          val globalRef :: path = parsePath(pathName)
          makeGlobalRefNativeLoadSpec(globalRef, path)
        }

        checkAndGetJSNativeLoadingSpecAnnotOf(pos, sym) match {
          case Some(annot) if annot.symbol == JSGlobalScopeAnnotation =>
            if (!sym.isModuleClass) {
              reporter.error(annot.pos,
                  "@JSGlobalScope can only be used on native JS objects (with @js.native).")
            }
            None

          case Some(annot) if annot.symbol == JSGlobalAnnotation =>
            if (shouldCheckLiterals)
              checkJSGlobalLiteral(annot)
            val pathName = annot.stringArg(0).getOrElse {
              val needsExplicitJSName = {
                (enclosingOwner is OwnerKind.ScalaMod) &&
                !sym.owner.isPackageObjectClass
              }

              if (needsExplicitJSName) {
                reporter.error(annot.pos,
                    "Native JS members inside non-native objects " +
                    "must have an explicit name in @JSGlobal")
              }
              jsInterop.defaultJSNameOf(sym)
            }
            Some(parseGlobalPath(pathName))

          case Some(annot) if annot.symbol == JSImportAnnotation =>
            if (shouldCheckLiterals)
              checkJSImportLiteral(annot)
            val module = annot.stringArg(0).getOrElse {
              "" // an error is reported by checkJSImportLiteral in this case
            }
            val path = annot.stringArg(1).fold[List[String]](Nil)(parsePath)
            val importSpec = Import(module, path)
            val loadSpec = annot.stringArg(2).fold[JSNativeLoadSpec] {
              importSpec
            } { globalPathName =>
              ImportWithGlobalFallback(importSpec,
                  parseGlobalPath(globalPathName))
            }
            Some(loadSpec)

          case Some(annot) =>
            abort(s"checkAndGetJSNativeLoadingSpecAnnotOf returned unexpected annotation $annot")

          case None =>
            /* We already emitted an error. Invent something not to cause
             * cascading errors.
             */
            Some(JSNativeLoadSpec.Global("erroneous", Nil))
        }
      }
    }

    /** Verify a ValOrDefDef that is annotated with `@js.native`. */
    private def transformJSNativeValOrDefDef(tree: ValOrDefDef): ValOrDefDef = {
      val sym = tree.symbol

      if (sym.isLazy || jsInterop.isJSSetter(sym)) {
        reporter.error(tree.pos,
            "@js.native is not allowed on vars, lazy vals and setter defs")
      } else if (jsInterop.isJSBracketAccess(sym)) {
        reporter.error(tree.pos,
            "@JSBracketAccess is not allowed on @js.native vals and defs")
      } else if (jsInterop.isJSBracketCall(sym)) {
        reporter.error(tree.pos,
            "@JSBracketCall is not allowed on @js.native vals and defs")
      }

      if (!sym.isAccessor)
        checkRHSCallsJSNative(tree, "@js.native members")

      if (sym.isMethod) { // i.e., it is not a field
        for (overridden <- sym.allOverriddenSymbols.headOption) {
          val verb = if (overridden.isDeferred) "implement" else "override"
          reporter.error(tree.pos,
              s"An @js.native member cannot $verb the inherited member " +
              overridden.fullName)
        }
      }

      tree
    }

    /** Verify a ValOrDefDef inside a js.Any */
    private def transformValOrDefDefInJSType(tree: ValOrDefDef): Tree = {
      val sym = tree.symbol

      assert(!sym.isLocalToBlock, s"$tree at ${tree.pos}")

      sym.name match {
        case nme.apply if !sym.hasAnnotation(JSNameAnnotation) && jsInterop.isJSGetter(sym) =>
          reporter.error(sym.pos, "A member named apply represents function " +
              "application in JavaScript. A parameterless member should be " +
              "exported as a property. You must add @JSName(\"apply\")")

        case nme.equals_ if sym.tpe.matches(Any_equals.tpe) =>
          reporter.warning(sym.pos, "Overriding equals in a JS class does " +
              "not change how it is compared. To silence this warning, change " +
              "the name of the method and optionally add @JSName(\"equals\").")

        case nme.hashCode_ if sym.tpe.matches(Any_hashCode.tpe) =>
          reporter.warning(sym.pos, "Overriding hashCode in a JS class does " +
              "not change its hash code. To silence this warning, change " +
              "the name of the method and optionally add @JSName(\"hashCode\").")

        case _ =>
      }

      if (jsInterop.isJSSetter(sym))
        checkSetterSignature(sym, tree.pos, exported = false)

      if (enclosingOwner is OwnerKind.JSNonNative) {
        JSCallingConvention.of(sym) match {
          case JSCallingConvention.Property(_) => // checked above
          case JSCallingConvention.Method(_)   => // no checks needed

          case JSCallingConvention.Call if !sym.isDeferred =>
            /* Concrete `def apply` methods are normally rejected because we
             * cannot implement them in JavaScript. However, we do allow a
             * synthetic `apply` method if it is in a SAM for a JS function
             * type.
             */
            val isJSFunctionSAM = {
              /* Under 2.11, sym.owner.isAnonymousFunction does not properly
               * recognize anonymous functions here (because they seem to not
               * be marked as synthetic).
               */
              sym.isSynthetic &&
              sym.owner.name == tpnme.ANON_FUN_NAME &&
              sym.owner.superClass == JSFunctionClass
            }
            if (!isJSFunctionSAM) {
              reporter.error(sym.pos,
                  "A non-native JS class cannot declare a concrete method " +
                  "named `apply` without `@JSName`")
            }

          case JSCallingConvention.Call => // if sym.isDeferred
            /* Allow an abstract `def apply` only if the owner is a plausible
             * JS function SAM trait.
             */
            val owner = sym.owner
            val isPlausibleJSFunctionType = {
              owner.isTrait &&
              owner.superClass == JSFunctionClass &&
              samOf(owner.toTypeConstructor) == sym
            }
            if (!isPlausibleJSFunctionType) {
              reporter.error(sym.pos,
                  "A non-native JS type can only declare an abstract " +
                  "method named `apply` without `@JSName` if it is the " +
                  "SAM of a trait that extends js.Function")
            }

          case JSCallingConvention.BracketAccess =>
            reporter.error(tree.pos,
                "@JSBracketAccess is not allowed in non-native JS classes")

          case JSCallingConvention.BracketCall =>
            reporter.error(tree.pos,
                "@JSBracketCall is not allowed in non-native JS classes")

          case JSCallingConvention.UnaryOp(_) =>
            reporter.error(sym.pos,
                "A non-native JS class cannot declare a method " +
                "named like a unary operation without `@JSName`")

          case JSCallingConvention.BinaryOp(_) =>
            reporter.error(sym.pos,
                "A non-native JS class cannot declare a method " +
                "named like a binary operation without `@JSName`")
        }
      } else {
        def checkNoDefaultOrRepeated(subject: String) = {
          for (param <- sym.paramss.flatten) {
            if (isScalaRepeatedParamType(param.tpe)) {
              reporter.error(param.pos, s"$subject may not have repeated parameters")
            } else if (param.isParamWithDefault) {
              reporter.error(param.pos, s"$subject may not have default parameters")
            }
          }
        }

        JSCallingConvention.of(sym) match {
          case JSCallingConvention.Property(_) => // checked above
          case JSCallingConvention.Method(_)   => // no checks needed
          case JSCallingConvention.Call        => // no checks needed
          case JSCallingConvention.UnaryOp(_)  => // no checks needed

          case JSCallingConvention.BinaryOp(_) =>
            checkNoDefaultOrRepeated("methods representing binary operations")

          case JSCallingConvention.BracketAccess =>
            val paramCount = sym.paramss.map(_.size).sum
            if (paramCount != 1 && paramCount != 2) {
              reporter.error(tree.pos,
                  "@JSBracketAccess methods must have one or two parameters")
            } else if (paramCount == 2 &&
                sym.tpe.finalResultType.typeSymbol != UnitClass) {
              reporter.error(tree.pos,
                  "@JSBracketAccess methods with two parameters must return Unit")
            }

            checkNoDefaultOrRepeated("@JSBracketAccess methods")

          case JSCallingConvention.BracketCall =>
            // JS bracket calls must have at least one non-repeated parameter
            sym.tpe.paramss match {
              case (param :: _) :: _ if !isScalaRepeatedParamType(param.tpe) =>
                // ok
              case _ =>
                reporter.error(tree.pos, "@JSBracketCall methods must have at " +
                    "least one non-repeated parameter")
            }
        }
      }

      if (sym.hasAnnotation(NativeAttr)) {
        // Native methods are not allowed
        reporter.error(tree.pos, "Methods in a js.Any may not be @native")
      }

      /* In native JS types, there should not be any private member, except
       * private[this] constructors.
       */
      if ((enclosingOwner is OwnerKind.JSNative) && isPrivateMaybeWithin(sym)) {
        // Necessary for `private[this] val/var
        def isFieldPrivateThis: Boolean = {
          sym.isPrivateThis &&
          !sym.isParamAccessor &&
          !sym.owner.info.decls.exists(s => s.isGetter && s.accessed == sym)
        }

        if (sym.isClassConstructor) {
          if (!sym.isPrivateThis) {
            reporter.error(sym.pos,
                "Native JS classes may not have private constructors. " +
                "Use `private[this]` to declare an internal constructor.")
          }
        } else if (sym.isMethod || isFieldPrivateThis) {
          reporter.error(tree.pos,
              "Native JS classes may not have private members. " +
              "Use a public member in a private facade instead.")
        }
      }

      if (enclosingOwner is OwnerKind.JSNonNative) {
        // Private methods cannot be overloaded
        if (sym.isMethod && isPrivateMaybeWithin(sym)) {
          val alts = sym.owner.info.member(sym.name).filter(_.isMethod)
          if (alts.isOverloaded) {
            reporter.error(tree.pos,
                "Private methods in non-native JS classes cannot be " +
                "overloaded. Use different names instead.")
          }
        }

        // private[Scope] methods must be final
        if (sym.isMethod && (sym.hasAccessBoundary && !sym.isProtected) &&
            !sym.isFinal && !sym.isClassConstructor) {
          reporter.error(tree.pos,
              "Qualified private members in non-native JS classes " +
              "must be final")
        }

        // Traits must be pure interfaces, except for js.undefined members
        if (sym.owner.isTrait && sym.isTerm && !sym.isConstructor) {
          if (sym.isMethod && isPrivateMaybeWithin(sym)) {
            reporter.error(tree.pos,
                "A non-native JS trait cannot contain private members")
          } else if (sym.isLazy) {
            reporter.error(tree.pos,
                "A non-native JS trait cannot contain lazy vals")
          } else if (!sym.isDeferred) {
            /* Tell the back-end not emit this thing. In fact, this only
             * matters for mixed-in members created from this member.
             */
            sym.addAnnotation(JSOptionalAnnotation)

            // For non-accessor methods, check that they do not have parens
            if (sym.isMethod && !sym.isAccessor) {
              sym.tpe match {
                case _: NullaryMethodType =>
                  // ok
                case PolyType(_, _: NullaryMethodType) =>
                  // ok
                case _ =>
                  reporter.error(tree.rhs.pos,
                      "In non-native JS traits, defs with parentheses " +
                      "must be abstract.")
              }
            }

            /* Check that the right-hand-side is `js.undefined`.
             *
             * On 2.12+, fields are created later than this phase, and getters
             * still hold the right-hand-side that we need to check (we
             * identify this case with `sym.accessed == NoSymbol`).
             * On 2.11 and before, however, the getter has already been
             * rewritten to read the field, so we must not check it.
             * In either case, setters must not be checked.
             */
            if (!sym.isAccessor || (sym.isGetter && sym.accessed == NoSymbol)) {
              // Check that the tree's body is `js.undefined`
              tree.rhs match {
                case sel: Select if sel.symbol == JSPackage_undefined =>
                  // ok
                case _ =>
                  if (sym.hasFlag(reflect.internal.Flags.DEFAULTPARAM)) {
                    reporter.error(tree.rhs.pos,
                        "Members of non-native JS traits may not have default " +
                        "parameters unless their default is `js.undefined`.")
                  } else {
                    reporter.error(tree.rhs.pos,
                        "Members of non-native JS traits must either be " +
                        "abstract, or their right-hand-side must be " +
                        "`js.undefined`.")
                  }
              }
            }
          }
        }
      }

      if (sym.isPrimaryConstructor || sym.isValueParameter ||
          sym.isParamWithDefault || sym.isAccessor ||
          sym.isParamAccessor || sym.isDeferred || sym.isSynthetic ||
          (enclosingOwner is OwnerKind.JSNonNative)) {
        /* Ignore (i.e. allow) primary ctor, parameters, default parameter
         * getters, accessors, param accessors, abstract members, synthetic
         * methods (to avoid double errors with case classes, e.g. generated
         * copy method), and any member of a non-native JS class/trait.
         */
      } else if (jsPrimitives.isJavaScriptPrimitive(sym)) {
        // No check for primitives. We trust our own standard library.
      } else if (sym.isConstructor) {
        // Force secondary ctor to have only a call to the primary ctor inside
        tree.rhs match {
          case Block(List(Apply(trg, _)), Literal(Constant(())))
              if trg.symbol.isPrimaryConstructor &&
                 trg.symbol.owner == sym.owner =>
            // everything is fine here
          case _ =>
            reporter.error(tree.pos, "A secondary constructor of a class " +
                "extending js.Any may only call the primary constructor")
        }
      } else {
        // Check that the tree's rhs is exactly `= js.native`
        checkRHSCallsJSNative(tree, "Concrete members of JS native types")
      }

      super.transform(tree)
    }

    private def checkRHSCallsJSNative(tree: ValOrDefDef,
        longKindStr: String): Unit = {
      // Check that the rhs is exactly `= js.native`
      tree.rhs match {
        case sel: Select if sel.symbol == JSPackage_native =>
          // ok
        case _ =>
          val pos = if (tree.rhs != EmptyTree) tree.rhs.pos else tree.pos
          reporter.error(pos, s"$longKindStr may only call js.native.")
      }

      // Warn if resultType is Nothing and not ascribed
      val sym = tree.symbol
      if (sym.tpe.resultType.typeSymbol == NothingClass &&
          tree.tpt.asInstanceOf[TypeTree].original == null) {
        val name = sym.name.decoded.trim
        reporter.warning(tree.pos,
            s"The type of $name got inferred as Nothing. " +
            "To suppress this warning, explicitly ascribe the type.")
      }
    }

    private def checkJSNativeSpecificAnnotsOnNonJSNative(
        memberDef: MemberDef): Unit = {
      val sym = memberDef.symbol

      for (annot <- sym.annotations) {
        annot.symbol match {
          case JSGlobalAnnotation =>
            reporter.error(annot.pos,
                "@JSGlobal can only be used on native JS definitions (with @js.native).")
          case JSImportAnnotation =>
            reporter.error(annot.pos,
                "@JSImport can only be used on native JS definitions (with @js.native).")
          case JSGlobalScopeAnnotation =>
            reporter.error(annot.pos,
                "@JSGlobalScope can only be used on native JS objects (with @js.native).")
          case _ =>
            // ok
        }
      }
    }

    private def checkJSNameAnnots(sym: Symbol): Unit = {
      for (annot <- sym.getAnnotation(JSNameAnnotation)) {
        // Check everything about the first @JSName annotation
        if (sym.isLocalToBlock || (enclosingOwner isnt OwnerKind.JSType)) {
          reporter.error(annot.pos,
              "@JSName can only be used on members of JS types.")
        } else if (sym.isTrait) {
          reporter.error(annot.pos,
              "@JSName cannot be used on traits.")
        } else if ((sym.isMethod || sym.isClass) && isPrivateMaybeWithin(sym)) {
          reporter.error(annot.pos,
              "@JSName cannot be used on private members.")
        } else {
          if (shouldCheckLiterals)
            checkJSNameArgument(sym, annot)
        }
      }
    }

    private def checkDuplicateJSMemberAnnots(sym: Symbol): Unit = {
      sym.annotations
        .filter(annot => JSMemberAnnots.contains(annot.symbol))
        .drop(1)
        .foreach { annot =>
          reporter.error(annot.pos, "A member can have at most one " +
              "annotation among @JSName, @JSBracketAccess and @JSBracketCall.")
        }
    }

    private lazy val JSMemberAnnots: Set[Symbol] =
      Set(JSNameAnnotation, JSBracketAccessAnnotation, JSBracketCallAnnotation)

    /** Checks that argument to @JSName on [[member]] is a literal.
     *  Reports an error on each annotation where this is not the case.
     */
    private def checkJSNameArgument(memberSym: Symbol, annot: AnnotationInfo): Unit = {
      val argTree = annot.args.head
      if (argTree.tpe.typeSymbol == StringClass) {
        if (annot.stringArg(0).isEmpty) {
          reporter.error(argTree.pos,
              "A string argument to JSName must be a literal string")
        }
      } else {
        // We have a js.Symbol
        val sym = argTree.symbol
        if (!sym.isStatic || !sym.isStable) {
          reporter.error(argTree.pos,
              "A js.Symbol argument to JSName must be a static, stable identifier")
        } else if ((enclosingOwner is OwnerKind.JSNonNative) &&
            sym.owner == memberSym.owner) {
          reporter.warning(argTree.pos,
              "This symbol is defined in the same object as the annotation's " +
              "target. This will cause a stackoverflow at runtime")
        }
      }
    }

    /** Mark the symbol as exposed if it is a non-private term member of a
     *  non-native JS class.
     *
     *  @param sym
     *    The symbol, which must be the module symbol for a module, not its
     *    module class symbol.
     */
    private def markExposedIfRequired(sym: Symbol): Unit = {
      def shouldBeExposed: Boolean = {
        // it is a member of a non-native JS class
        (enclosingOwner is OwnerKind.JSNonNative) && !sym.isLocalToBlock &&
        // it is a term member
        (sym.isModule || sym.isMethod) &&
        // it is not private
        !isPrivateMaybeWithin(sym) &&
        // it is not a kind of term member that we never expose
        !sym.isConstructor && !sym.isValueParameter && !sym.isParamWithDefault &&
        // it is not synthetic
        !sym.isSynthetic
      }

      if (shouldPrepareExports && shouldBeExposed) {
        sym.addAnnotation(ExposedJSMemberAnnot)
        /* For accessors, the field being accessed must also be exposed,
         * although it is private.
         *
         * #4089 Don't do this if `sym.accessed == NoSymbol`. This happens in
         * 2.12+, where fields are created later than this phase.
         */
        if (sym.isAccessor && sym.accessed != NoSymbol)
          sym.accessed.addAnnotation(ExposedJSMemberAnnot)
      }
    }

  }

  def isJSAny(sym: Symbol): Boolean =
    sym.isSubClass(JSAnyClass)

  /** Checks that a setter has the right signature.
   *
   *  Reports error messages otherwise.
   */
  def checkSetterSignature(sym: Symbol, pos: Position, exported: Boolean): Unit = {
    val typeStr = if (exported) "Exported" else "JS"

    // Forbid setters with non-unit return type
    if (sym.tpe.resultType.typeSymbol != UnitClass) {
      reporter.error(pos, s"$typeStr setters must return Unit")
    }

    // Forbid setters with more than one argument
    sym.tpe.paramss match {
      case List(List(arg)) =>
        // Arg list is OK. Do additional checks.
        if (isScalaRepeatedParamType(arg.tpe))
          reporter.error(pos, s"$typeStr setters may not have repeated params")

        if (arg.hasFlag(reflect.internal.Flags.DEFAULTPARAM))
          reporter.error(pos, s"$typeStr setters may not have default params")

      case _ =>
        reporter.error(pos, s"$typeStr setters must have exactly one argument")
    }
  }

  /** Tests whether the symbol has `private` in any form, either `private`,
   *  `private[this]` or `private[Enclosing]`.
   */
  def isPrivateMaybeWithin(sym: Symbol): Boolean =
    sym.isPrivate || (sym.hasAccessBoundary && !sym.isProtected)

  /** Checks that the optional argument to an `@JSGlobal` annotation is a
   *  literal.
   *
   *  Reports an error on the annotation if it is not the case.
   */
  private def checkJSGlobalLiteral(annot: AnnotationInfo): Unit = {
    if (annot.args.nonEmpty) {
      assert(annot.args.size == 1,
          s"@JSGlobal annotation $annot has more than 1 argument")

      val argIsValid = annot.stringArg(0).isDefined
      if (!argIsValid) {
        reporter.error(annot.args.head.pos,
            "The argument to @JSGlobal must be a literal string.")
      }
    }
  }

  /** Checks that arguments to an `@JSImport` annotation are literals.
   *
   *  The second argument can also be the singleton `JSImport.Namespace`
   *  object.
   *
   *  Reports an error on the annotation if it is not the case.
   */
  private def checkJSImportLiteral(annot: AnnotationInfo): Unit = {
    assert(annot.args.size == 2 || annot.args.size == 3,
        s"@JSImport annotation $annot does not have exactly 2 or 3 arguments")

    val firstArgIsValid = annot.stringArg(0).isDefined
    if (!firstArgIsValid) {
      reporter.error(annot.args.head.pos,
          "The first argument to @JSImport must be a literal string.")
    }

    val secondArgIsValid = {
      annot.stringArg(1).isDefined ||
      annot.args(1).symbol == JSImportNamespaceObject
    }
    if (!secondArgIsValid) {
      reporter.error(annot.args(1).pos,
          "The second argument to @JSImport must be literal string or the " +
          "JSImport.Namespace object.")
    }

    val thirdArgIsValid = annot.args.size < 3 || annot.stringArg(2).isDefined
    if (!thirdArgIsValid) {
      reporter.error(annot.args(2).pos,
          "The third argument to @JSImport, when present, must be a " +
          "literal string.")
    }
  }

  private abstract class ScalaEnumFctExtractors(methSym: Symbol) {
    private def resolve(ptpes: Symbol*) = {
      val res = methSym suchThat {
        _.tpe.params.map(_.tpe.typeSymbol) == ptpes.toList
      }
      assert(res != NoSymbol, s"no overload of $methSym for param types $ptpes")
      res
    }

    private val noArg = resolve()
    private val nameArg = resolve(StringClass)
    private val intArg = resolve(IntClass)
    private val fullMeth = resolve(IntClass, StringClass)

    /**
     * Extractor object for calls to the targeted symbol that do not have an
     * explicit name in the parameters
     *
     * Extracts:
     * - `sel: Select` where sel.symbol is targeted symbol (no arg)
     * - Apply(meth, List(param)) where meth.symbol is targeted symbol (i: Int)
     */
    object NoName {
      def unapply(t: Tree): Option[Option[Tree]] = t match {
        case sel: Select if sel.symbol == noArg =>
          Some(None)
        case Apply(meth, List(param)) if meth.symbol == intArg =>
          Some(Some(param))
        case _ =>
          None
      }
    }

    object NullName {
      def unapply(tree: Tree): Boolean = tree match {
        case Apply(meth, List(Literal(Constant(null)))) =>
          meth.symbol == nameArg
        case Apply(meth, List(_, Literal(Constant(null)))) =>
          meth.symbol == fullMeth
        case _ => false
      }
    }

  }

  private object ScalaEnumValue
      extends ScalaEnumFctExtractors(getMemberMethod(ScalaEnumClass, jsnme.Value))

  private object ScalaEnumVal
      extends ScalaEnumFctExtractors(getMemberClass(ScalaEnumClass, jsnme.Val).tpe.member(nme.CONSTRUCTOR))

  /**
   * Construct a call to Enumeration.Value
   * @param thisSym  ClassSymbol of enclosing class
   * @param nameOrig Symbol of ValDef where this call will be placed
   *                 (determines the string passed to Value)
   * @param intParam Optional tree with Int passed to Value
   * @return Typed tree with appropriate call to Value
   */
  private def ScalaEnumValName(
      thisSym: Symbol,
      nameOrig: Symbol,
      intParam: Option[Tree]) = {

    val defaultName = nameOrig.asTerm.getterName.encoded


    // Construct the following tree
    //
    //   if (nextName != null && nextName.hasNext)
    //     nextName.next()
    //   else
    //     <defaultName>
    //
    val nextNameTree = Select(This(thisSym), jsnme.nextName)
    val nullCompTree =
      Apply(Select(nextNameTree, nme.NE), Literal(Constant(null)) :: Nil)
    val hasNextTree = Select(nextNameTree, jsnme.hasNext)
    val condTree = Apply(Select(nullCompTree, nme.ZAND), hasNextTree :: Nil)
    val nameTree = If(condTree,
        Apply(Select(nextNameTree, jsnme.next), Nil),
        Literal(Constant(defaultName)))
    val params = intParam.toList :+ nameTree

    typer.typed {
      Apply(Select(This(thisSym), jsnme.Value), params)
    }
  }

  private def checkAndGetJSNativeLoadingSpecAnnotOf(
      pos: Position, sym: Symbol): Option[Annotation] = {

    for (annot <- sym.getAnnotation(JSNameAnnotation)) {
      reporter.error(annot.pos,
          "@JSName can only be used on members of JS types.")
    }

    val annots = sym.annotations.filter { annot =>
      JSNativeLoadingSpecAnnots.contains(annot.symbol)
    }

    val badAnnotCountMsg = if (sym.isModuleClass) {
      "Native JS objects must have exactly one annotation among " +
      "@JSGlobal, @JSImport and @JSGlobalScope."
    } else {
      "Native JS classes, vals and defs must have exactly one annotation " +
      "among @JSGlobal and @JSImport."
    }

    annots match {
      case Nil =>
        reporter.error(pos, badAnnotCountMsg)
        None

      case result :: duplicates =>
        for (annot <- duplicates)
          reporter.error(annot.pos, badAnnotCountMsg)

        Some(result)
    }
  }

  /* Note that we consider @JSGlobalScope as a JS native loading spec because
   * it's convenient for the purposes of PrepJSInterop. Actually @JSGlobalScope
   * objects do not receive a JS loading spec in their IR.
   */
  private lazy val JSNativeLoadingSpecAnnots: Set[Symbol] = {
    Set(JSGlobalAnnotation, JSImportAnnotation, JSGlobalScopeAnnotation)
  }

  private lazy val ScalaEnumClass = getRequiredClass("scala.Enumeration")

  private def wasPublicBeforeTyper(sym: Symbol): Boolean =
    sym.hasAnnotation(WasPublicBeforeTyperClass)

  private def fixPublicBeforeTyper(ddef: DefDef): DefDef = {
    // This method assumes that isJSAny(ddef.symbol.owner) is true
    val sym = ddef.symbol
    val needsFix = {
      sym.isPrivate &&
      (wasPublicBeforeTyper(sym) ||
          (sym.isAccessor && wasPublicBeforeTyper(sym.accessed)))
    }
    if (needsFix) {
      sym.resetFlag(Flag.PRIVATE)
      treeCopy.DefDef(ddef, ddef.mods &~ Flag.PRIVATE, ddef.name, ddef.tparams,
          ddef.vparamss, ddef.tpt, ddef.rhs)
    } else {
      ddef
    }
  }

  private def checkInternalAnnotations(sym: Symbol): Unit = {
    /** Returns true iff it is a compiler annotations. This does not include
     *  annotations inserted before the typer (such as `@WasPublicBeforeTyper`).
     */
    def isCompilerAnnotation(annotation: AnnotationInfo): Boolean = {
      annotation.symbol == ExposedJSMemberAnnot ||
      annotation.symbol == JSTypeAnnot ||
      annotation.symbol == JSOptionalAnnotation
    }

    for (annotation <- sym.annotations) {
      if (isCompilerAnnotation(annotation)) {
        reporter.error(annotation.pos,
            s"$annotation is for compiler internal use only. " +
            "Do not use it yourself.")
      }
    }
  }

  private def moduleToModuleClass(sym: Symbol): Symbol =
    if (sym.isModule) sym.moduleClass
    else sym
}

object PrepJSInterop {
  private final class OwnerKind private (private val baseKinds: Int)
      extends AnyVal {

    import OwnerKind._

    @inline def isBaseKind: Boolean =
      Integer.lowestOneBit(baseKinds) == baseKinds && baseKinds != 0 // exactly 1 bit on

    @inline def |(that: OwnerKind): OwnerKind =
      new OwnerKind(this.baseKinds | that.baseKinds)

    @inline def is(that: OwnerKind): Boolean =
      (this.baseKinds & that.baseKinds) != 0

    @inline def isnt(that: OwnerKind): Boolean =
      !this.is(that)
  }

  private object OwnerKind {
    /** No owner, i.e., we are at the top-level. */
    val None = new OwnerKind(0x00)

    // Base kinds - those form a partition of all possible enclosing owners

    /** A Scala class/trait that does not extend Enumeration. */
    val NonEnumScalaClass = new OwnerKind(0x01)
    /** A Scala object that does not extend Enumeration. */
    val NonEnumScalaMod = new OwnerKind(0x02)
    /** A native JS class/trait, which extends js.Any. */
    val JSNativeClass = new OwnerKind(0x04)
    /** A native JS object, which extends js.Any. */
    val JSNativeMod = new OwnerKind(0x08)
    /** A non-native JS class/trait. */
    val JSClass = new OwnerKind(0x10)
    /** A non-native JS object. */
    val JSMod = new OwnerKind(0x20)
    /** A Scala class/trait that extends Enumeration. */
    val EnumClass = new OwnerKind(0x40)
    /** A Scala object that extends Enumeration. */
    val EnumMod = new OwnerKind(0x80)
    /** The Enumeration class itself. */
    val EnumImpl = new OwnerKind(0x100)

    // Compound kinds

    /** A Scala class/trait, possibly Enumeration-related. */
    val ScalaClass = NonEnumScalaClass | EnumClass | EnumImpl
    /** A Scala object, possibly Enumeration-related. */
    val ScalaMod = NonEnumScalaMod | EnumMod
    /** A Scala class, trait or object, i.e., anything not extending js.Any. */
    val ScalaThing = ScalaClass | ScalaMod

    /** A Scala class/trait/object extending Enumeration, but not Enumeration itself. */
    val Enum = EnumClass | EnumMod

    /** A native JS class/trait/object. */
    val JSNative = JSNativeClass | JSNativeMod
    /** A non-native JS class/trait/object. */
    val JSNonNative = JSClass | JSMod
    /** A JS type, i.e., something extending js.Any. */
    val JSType = JSNative | JSNonNative

    /** Any kind of class/trait, i.e., a Scala or JS class/trait. */
    val AnyClass = ScalaClass | JSNativeClass | JSClass
  }
}
