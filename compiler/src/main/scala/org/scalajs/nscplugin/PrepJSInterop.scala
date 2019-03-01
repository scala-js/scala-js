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

import org.scalajs.ir.Trees.{isValidIdentifier, JSNativeLoadSpec}

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
  import jsInterop.JSName

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

    // Force evaluation of JSDynamicLiteral: Strangely, we are unable to find
    // nested objects in the JSCode phase (probably after flatten).
    // Therefore we force the symbol of js.Dynamic.literal here in order to
    // have access to it in JSCode.
    JSDynamicLiteral

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
      checkInternalAnnotations(tree)

      val preTransformedTree = tree match {
        // Handle js.Anys
        case idef: ImplDef if isJSAny(idef) =>
          transformJSAny(idef)

        // In native JS things, only js.Any stuff is allowed
        case idef: ImplDef if enclosingOwner is OwnerKind.JSNative =>
          /* We have to allow synthetic companion objects here, as they get
           * generated when a nested native JS class has default arguments in
           * its constructor (see #1891).
           */
          if (!idef.symbol.isSynthetic) {
            reporter.error(idef.pos,
                "Native JS traits, classes and objects cannot contain inner " +
                "Scala traits, classes or objects (i.e., not extending js.Any)")
          }
          super.transform(tree)

        // Catch the definition of scala.Enumeration itself
        case cldef: ClassDef if cldef.symbol == ScalaEnumClass =>
          enterOwner(OwnerKind.EnumImpl) { super.transform(cldef) }

        // Catch Scala Enumerations to transform calls to scala.Enumeration.Value
        case idef: ImplDef if isScalaEnum(idef) =>
          val sym = idef.symbol

          checkJSAnySpecificAnnotsOnNonJSAny(idef)

          val kind =
            if (idef.isInstanceOf[ModuleDef]) OwnerKind.EnumMod
            else OwnerKind.EnumClass
          enterOwner(kind) { super.transform(idef) }

        // Catch (Scala) ClassDefs to forbid js.Anys
        case cldef: ClassDef =>
          val sym = cldef.symbol

          checkJSAnySpecificAnnotsOnNonJSAny(cldef)

          if (sym == UnionClass)
            sym.addAnnotation(JSTypeAnnot)

          if (shouldPrepareExports)
            registerClassOrModuleExports(sym)

          enterOwner(OwnerKind.NonEnumScalaClass) { super.transform(cldef) }

        // Module export sanity check (export generated in JSCode phase)
        case modDef: ModuleDef =>
          val sym = modDef.symbol

          checkJSAnySpecificAnnotsOnNonJSAny(modDef)

          if (shouldPrepareExports)
            registerClassOrModuleExports(sym.moduleClass)

          enterOwner(OwnerKind.NonEnumScalaMod) { super.transform(modDef) }

        // ValOrDefDef's that are local to a block must not be transformed
        case vddef: ValOrDefDef if vddef.symbol.isLocalToBlock =>
          super.transform(tree)

        // Catch ValDef in js.Any
        case vdef: ValDef if enclosingOwner is OwnerKind.JSType =>
          transformValOrDefDefInJSType(vdef)

        // Catch DefDef in js.Any
        case ddef: DefDef if enclosingOwner is OwnerKind.JSType =>
          transformValOrDefDefInJSType(fixPublicBeforeTyper(ddef))

        // Catch ValDefs in enumerations with simple calls to Value
        case ValDef(mods, name, tpt, ScalaEnumValue.NoName(optPar))
            if anyEnclosingOwner is OwnerKind.Enum =>
          val nrhs = ScalaEnumValName(tree.symbol.owner, tree.symbol, optPar)
          treeCopy.ValDef(tree, mods, name, transform(tpt), nrhs)

        // Exporter generation
        case _: ValOrDefDef if tree.symbol.isMethod =>
          val sym = tree.symbol
          if (shouldPrepareExports) {
            // Generate exporters for this ddef if required
            exporters.getOrElseUpdate(sym.owner,
                mutable.ListBuffer.empty) ++= genExportMember(sym)
          }
          super.transform(tree)

        /* Anonymous function, need to check that it is not used as a SAM for a
         * JS type, unless it is js.FunctionN or js.ThisFunctionN.
         * See #2921.
         */
        case tree: Function =>
          val tpeSym = tree.tpe.typeSymbol
          if (isJSAny(tpeSym) && !AllJSFunctionClasses.contains(tpeSym)) {
            reporter.error(tree.pos,
                "Using an anonymous function as a SAM for the JavaScript " +
                "type " + tpeSym.fullNameString + " is not allowed. " +
                "Use an anonymous class instead.")
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
            if (typer.checkClassType(tpeArg)) {
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

      postTransform(preTransformedTree)
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

    private def postTransform(tree: Tree) = tree match {
      case _ if !shouldPrepareExports =>
        tree

      case Template(parents, self, body) =>
        val clsSym = tree.symbol.owner

        // Check that @JSExportStatic fields come first
        if (clsSym.isModuleClass) { // quick check to avoid useless work
          var foundStatOrNonStaticVal: Boolean = false
          for (tree <- body) {
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

        val exports = exporters.get(clsSym).toIterable.flatten
        // Add exports to the template
        treeCopy.Template(tree, parents, self, body ++ exports)

      case memDef: MemberDef =>
        val sym = memDef.symbol
        if (shouldPrepareExports && sym.isLocalToBlock) {
          // Exports are never valid on local definitions, but delegate complaining.
          val exports = genExportMember(sym)
          assert(exports.isEmpty, "Generated exports for local definition.")
        }

        // Expose objects (modules) members of non-native JS classes
        if (sym.isModule && (enclosingOwner is OwnerKind.JSNonNative)) {
          if (shouldModuleBeExposed(sym))
            sym.addAnnotation(ExposedJSMemberAnnot)
        }

        memDef

      case _ => tree
    }

    /**
     * Performs checks and rewrites specific to classes / objects extending
     * js.Any
     */
    private def transformJSAny(implDef: ImplDef) = {
      val sym = implDef match {
        case _: ModuleDef => implDef.symbol.moduleClass
        case _            => implDef.symbol
      }

      lazy val badParent = sym.info.parents find { t =>
        /* We have to allow scala.Dynamic to be able to define js.Dynamic
         * and similar constructs. This causes the unsoundness filed as #1385.
         */
        !(t <:< JSAnyClass.tpe || t =:= AnyRefClass.tpe || t =:= DynamicClass.tpe)
      }

      def isNativeJSTraitType(tpe: Type): Boolean = {
        val sym = tpe.typeSymbol
        sym.isTrait && sym.hasAnnotation(JSNativeAnnotation)
      }

      val isJSAnonFun = isJSLambda(sym)

      sym.addAnnotation(JSTypeAnnot)

      /* Anonymous functions are considered native, since they are handled
       * specially in the backend.
       */
      val isJSNative = sym.hasAnnotation(JSNativeAnnotation) || isJSAnonFun

      // Forbid @EnableReflectiveInstantiation on JS types
      sym.getAnnotation(EnableReflectiveInstantiationAnnotation).foreach {
        annot =>
          reporter.error(annot.pos,
              "@EnableReflectiveInstantiation cannot be used on types " +
              "extending js.Any.")
      }

      if (sym.isPackageObjectClass)
        reporter.error(implDef.pos, "Package objects may not extend js.Any.")

      def strKind =
        if (sym.isTrait) "trait"
        else if (sym.isModuleClass) "object"
        else "class"

      // Check that we do not have a case modifier
      if (implDef.mods.hasFlag(Flag.CASE)) {
        reporter.error(implDef.pos, "Classes and objects extending " +
            "js.Any may not have a case modifier")
      }

      // Check that we do not extend a trait that does not extends js.Any
      if (!isJSAnonFun && badParent.isDefined) {
        val badName = badParent.get.typeSymbol.fullName
        reporter.error(implDef.pos, s"${sym.nameString} extends ${badName} " +
            "which does not extend js.Any.")
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

        // Unless it is a trait, it cannot inherit directly from AnyRef
        if (!sym.isTrait && sym.info.parents.exists(_ =:= AnyRefClass.tpe)) {
          reporter.error(implDef.pos,
              s"A non-native JS $strKind cannot directly extend AnyRef. " +
              "It must extend a JS class (native or not).")
        }

        // Check that we do not inherit directly from a native JS trait
        if (sym.info.parents.exists(isNativeJSTraitType)) {
          reporter.error(implDef.pos,
              s"A non-native JS $strKind cannot directly extend a "+
              "native JS trait.")
        }

        // Local JS classes cannot be abstract (implementation restriction)
        if (!sym.isTrait && sym.isAbstractClass && sym.isLocalToBlock) {
          reporter.error(implDef.pos,
              "Implementation restriction: local JS classes cannot be abstract")
        }

        // Check that there is no JS-native-specific annotation
        checkJSNativeSpecificAnnotsOnNonJSNative(implDef)
      }

      if (shouldCheckLiterals) {
        checkJSNameArgument(implDef)
        checkJSGlobalLiteral(sym)
        checkJSImportLiteral(sym)
      }

      // Checks for native JS stuff, excluding JS anon functions
      if (isJSNative && !isJSAnonFun) {
        // Check if we may have a JS native here
        if (sym.isLocalToBlock) {
          reporter.error(implDef.pos,
              "Local native JS classes and objects are not allowed")
        } else if (anyEnclosingOwner is OwnerKind.ScalaClass) {
          reporter.error(implDef.pos,
              "Scala traits and classes may not have inner native JS " +
              "traits, classes or objects")
        } else if (enclosingOwner is OwnerKind.JSNonNative) {
          reporter.error(implDef.pos, "non-native JS classes, traits and " +
              "objects may not have inner native JS classes, traits or objects")
        } else if (!sym.isTrait) {
          /* Compute the loading spec now, before `flatten` destroys the name.
           * We store it in a global map.
           */
          val optLoadSpec = checkAndComputeJSNativeLoadSpecOf(implDef.pos, sym)
          for (loadSpec <- optLoadSpec)
            jsInterop.storeJSNativeLoadSpec(sym, loadSpec)
        } else {
          assert(sym.isTrait, sym) // just tested in the previous `if`
          for (annot <- sym.annotations) {
            val annotSym = annot.symbol
            if (JSNativeLoadingSpecAnnots.contains(annotSym) ||
                annotSym == JSNameAnnotation) {
              reporter.error(annot.pos,
                  s"Traits may not have an @${annotSym.nameString} annotation.")
            }
          }
        }
      }

      if (shouldPrepareExports)
        registerClassOrModuleExports(sym)

      // Check for consistency of JS semantics across overriding
      for (overridingPair <- new overridingPairs.Cursor(sym).iterator) {
        val low = overridingPair.low
        val high = overridingPair.high

        def errorPos = {
          if (sym == low.owner) low.pos
          else if (sym == high.owner) high.pos
          else sym.pos
        }

        def memberDefString(membSym: Symbol): String =
          membSym.defStringSeenAs(sym.thisType.memberType(membSym))

        // Check for overrides with different JS names - issue #1983
        if (jsInterop.jsNameOf(low) != jsInterop.jsNameOf(high)) {
          val msg = {
            def memberDefStringWithJSName(membSym: Symbol) = {
              memberDefString(membSym) +
              membSym.locationString + " with JSName '" +
              jsInterop.jsNameOf(membSym).displayName + '\''
            }
            "A member of a JS class is overriding another member with a different JS name.\n\n" +
            memberDefStringWithJSName(low) + "\n" +
            "    is conflicting with\n" +
            memberDefStringWithJSName(high) + "\n"
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

      val kind = {
        if (!isJSNative) {
          if (sym.isModuleClass) OwnerKind.JSMod
          else OwnerKind.JSClass
        } else {
          if (sym.isModuleClass) OwnerKind.JSNativeMod
          else OwnerKind.JSNativeClass
        }
      }
      enterOwner(kind) { super.transform(implDef) }
    }

    private def checkAndComputeJSNativeLoadSpecOf(pos: Position,
        sym: Symbol): Option[JSNativeLoadSpec] = {
      import JSNativeLoadSpec._

      if (enclosingOwner is OwnerKind.JSNative) {
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

          val ownerLoadSpec = jsInterop.jsNativeLoadSpecOf(sym.owner)
          val loadSpec = ownerLoadSpec match {
            case Global(globalRef, path) =>
              Global(globalRef, path :+ jsName)
            case Import(module, path) =>
              Import(module, path :+ jsName)
            case ImportWithGlobalFallback(
                Import(module, modulePath), Global(globalRef, globalPath)) =>
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
          if (!isValidIdentifier(globalRef)) {
            reporter.error(pos,
                "The name of a JS global variable must be a valid JS " +
                s"identifier (got '$globalRef')")
          }
          JSNativeLoadSpec.Global(globalRef, path)
        }

        checkAndGetJSNativeLoadingSpecAnnotOf(pos, sym) match {
          case Some(annot) if annot.symbol == JSGlobalScopeAnnotation =>
            if (!sym.isModuleClass) {
              reporter.error(annot.pos,
                  "Only native JS objects can have an @JSGlobalScope annotation.")
            }
            None

          case Some(annot) if annot.symbol == JSGlobalAnnotation =>
            val pathName = annot.stringArg(0).getOrElse {
              val needsExplicitJSName = {
                (enclosingOwner is OwnerKind.ScalaMod) &&
                !sym.owner.isPackageObjectClass
              }

              if (needsExplicitJSName) {
                reporter.error(annot.pos,
                    "Native JS classes and objects inside non-native objects " +
                    "must have an explicit name in @JSGlobal")
              }
              jsInterop.defaultJSNameOf(sym)
            }
            Some(parseGlobalPath(pathName))

          case Some(annot) if annot.symbol == JSImportAnnotation =>
            val module = annot.stringArg(0).getOrElse {
              "" // do not care because it does not compile anyway
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

          case None =>
            // We already emitted an error. Just propagate something.
            None
        }
      }
    }

    /** Verify a ValOrDefDef inside a js.Any */
    private def transformValOrDefDefInJSType(tree: ValOrDefDef) = {
      val sym = tree.symbol

      assert(!sym.isLocalToBlock, s"$tree at ${tree.pos}")

      if (shouldPrepareExports) {
        // Exports are never valid on members of JS types, but delegate
        // complaining.
        val exports = genExportMember(sym)
        assert(exports.isEmpty, "Generated exports for member JS type.")

        /* Add the @ExposedJSMember annotation to exposed symbols in
         * non-native JS classes.
         */
        if (enclosingOwner is OwnerKind.JSNonNative) {
          def shouldBeExposed: Boolean = {
            !sym.isConstructor &&
            !sym.isValueParameter &&
            !sym.isParamWithDefault &&
            !sym.isSynthetic &&
            !isPrivateMaybeWithin(sym)
          }
          if (shouldBeExposed) {
            sym.addAnnotation(ExposedJSMemberAnnot)

            /* The field being accessed must also be exposed, although it's
             * private.
             */
            if (sym.isAccessor)
              sym.accessed.addAnnotation(ExposedJSMemberAnnot)
          }
        }
      }

      /* If this is an accessor, copy a potential @JSName annotation from
       * the field since otherwise it will get lost for traits (since they
       * have no fields).
       *
       * Do this only if the accessor does not already have an @JSName itself
       * (this happens almost all the time now that @JSName is annotated with
       * @field @getter @setter).
       */
      if (sym.isAccessor && !sym.hasAnnotation(JSNameAnnotation))
        sym.accessed.getAnnotation(JSNameAnnotation).foreach(sym.addAnnotation)

      if (sym.name == nme.apply && !sym.hasAnnotation(JSNameAnnotation)) {
        if (jsInterop.isJSGetter(sym)) {
          reporter.error(sym.pos, s"A member named apply represents function " +
              "application in JavaScript. A parameterless member should be " +
              "exported as a property. You must add @JSName(\"apply\")")
        } else if (enclosingOwner is OwnerKind.JSNonNative) {
          reporter.error(sym.pos,
              "A non-native JS class cannot declare a method " +
              "named `apply` without `@JSName`")
        }
      }

      if (jsInterop.isJSSetter(sym))
        checkSetterSignature(sym, tree.pos, exported = false)

      if (jsInterop.isJSBracketAccess(sym)) {
        if (enclosingOwner is OwnerKind.JSNonNative) {
          reporter.error(tree.pos,
              "@JSBracketAccess is not allowed in non-native JS classes")
        } else {
          val paramCount = sym.paramss.map(_.size).sum
          if (paramCount != 1 && paramCount != 2) {
            reporter.error(tree.pos,
                "@JSBracketAccess methods must have one or two parameters")
          } else if (paramCount == 2 &&
              sym.tpe.finalResultType.typeSymbol != UnitClass) {
            reporter.error(tree.pos,
                "@JSBracketAccess methods with two parameters must return Unit")
          }

          for (param <- sym.paramss.flatten) {
            if (isScalaRepeatedParamType(param.tpe)) {
              reporter.error(param.pos,
                  "@JSBracketAccess methods may not have repeated parameters")
            } else if (param.isParamWithDefault) {
              reporter.error(param.pos,
                  "@JSBracketAccess methods may not have default parameters")
            }
          }
        }
      }

      if (jsInterop.isJSBracketCall(sym)) {
        if (enclosingOwner is OwnerKind.JSNonNative) {
          reporter.error(tree.pos,
              "@JSBracketCall is not allowed in non-native JS classes")
        } else {
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

      if (sym.hasAnnotation(JSGlobalAnnotation)) {
        reporter.error(tree.pos,
            "Methods and fields cannot be annotated with @JSGlobal.")
      } else if (sym.hasAnnotation(JSImportAnnotation)) {
        reporter.error(tree.pos,
            "Methods and fields cannot be annotated with @JSImport.")
      }

      if (shouldCheckLiterals)
        checkJSNameArgument(tree)

      // Check that there is at most one @JSName annotation.
      val allJSNameAnnots = sym.annotations.filter(_.symbol == JSNameAnnotation)
      for (duplicate <- allJSNameAnnots.drop(1)) { // does not throw if empty
        reporter.error(duplicate.pos,
            "A member can only have a single @JSName annotation.")
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
                  reporter.error(tree.rhs.pos,
                      "Members of non-native JS traits must either be " +
                      "abstract, or their right-hand-side must be " +
                      "`js.undefined`.")
              }
            }
          }
        }
      }

      if (sym.isPrimaryConstructor || sym.isValueParameter ||
          sym.isParamWithDefault || sym.isAccessor ||
          sym.isParamAccessor || sym.isDeferred || sym.isSynthetic ||
          AllJSFunctionClasses.contains(sym.owner) ||
          (enclosingOwner is OwnerKind.JSNonNative)) {
        /* Ignore (i.e. allow) primary ctor, parameters, default parameter
         * getters, accessors, param accessors, abstract members, synthetic
         * methods (to avoid double errors with case classes, e.g. generated
         * copy method), js.Functions and js.ThisFunctions (they need abstract
         * methods for SAM treatment), and any member of a non-native JS
         * class/trait.
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
        // Check that the tree's body is either empty or calls js.native
        tree.rhs match {
          case sel: Select if sel.symbol == JSPackage_native =>
          case _ =>
            val pos = if (tree.rhs != EmptyTree) tree.rhs.pos else tree.pos
            reporter.error(pos,
                "Concrete members of JS native types may only call js.native.")
        }

        if (sym.tpe.resultType.typeSymbol == NothingClass &&
            tree.tpt.asInstanceOf[TypeTree].original == null) {
          // Warn if resultType is Nothing and not ascribed
          val name = sym.name.decoded.trim
          reporter.warning(tree.pos, s"The type of $name got inferred " +
              "as Nothing. To suppress this warning, explicitly ascribe " +
              "the type.")
        }
      }

      super.transform(tree)
    }

    private def checkJSAnySpecificAnnotsOnNonJSAny(implDef: ImplDef): Unit = {
      val sym = implDef.symbol

      if (sym.hasAnnotation(JSNativeAnnotation)) {
        reporter.error(implDef.pos,
            "Classes, traits and objects not extending js.Any may not have an " +
            "@js.native annotation")
      } else {
        checkJSNativeSpecificAnnotsOnNonJSNative(implDef)
      }
    }

    private def checkJSNativeSpecificAnnotsOnNonJSNative(
        implDef: ImplDef): Unit = {
      val sym = implDef.symbol

      val allowJSName = {
        sym.isModuleOrModuleClass &&
        (enclosingOwner is OwnerKind.JSNonNative) &&
        shouldModuleBeExposed(sym)
      }

      for (annot <- sym.annotations) {
        if (annot.symbol == JSNameAnnotation && !allowJSName) {
          reporter.error(annot.pos,
              "Non JS-native classes, traits and objects may not have an " +
              "@JSName annotation.")
        } else if (annot.symbol == JSGlobalAnnotation) {
          reporter.error(annot.pos,
              "Non JS-native classes, traits and objects may not have an " +
              "@JSGlobal annotation.")
        } else if (annot.symbol == JSImportAnnotation) {
          reporter.error(annot.pos,
              "Non JS-native classes, traits and objects may not have an " +
              "@JSImport annotation.")
        } else if (annot.symbol == JSGlobalScopeAnnotation) {
          reporter.error(annot.pos,
              "Only native JS objects can have an @JSGlobalScope annotation.")
        }
      }
    }

    /** Checks that argument to @JSName on [[member]] is a literal.
     *  Reports an error on each annotation where this is not the case.
     */
    private def checkJSNameArgument(member: MemberDef): Unit = {
      for (annot <- member.symbol.getAnnotation(JSNameAnnotation)) {
        val argTree = annot.args.head
        if (argTree.tpe.typeSymbol == StringClass) {
          if (!argTree.isInstanceOf[Literal]) {
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
              sym.owner == member.symbol.owner) {
            reporter.warning(argTree.pos,
                "This symbol is defined in the same object as the annotation's " +
                "target. This will cause a stackoverflow at runtime")
          }
        }

      }
    }

  }

  def isJSAny(sym: Symbol): Boolean =
    sym.tpe.typeSymbol isSubClass JSAnyClass

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

  private def isJSAny(implDef: ImplDef): Boolean = isJSAny(implDef.symbol)

  private def isJSLambda(sym: Symbol) = sym.isAnonymousClass &&
    AllJSFunctionClasses.exists(sym.tpe.typeSymbol isSubClass _)

  private def isScalaEnum(implDef: ImplDef) =
    implDef.symbol.tpe.typeSymbol isSubClass ScalaEnumClass

  /** Tests whether the symbol has `private` in any form, either `private`,
   *  `private[this]` or `private[Enclosing]`.
   */
  def isPrivateMaybeWithin(sym: Symbol): Boolean =
    sym.isPrivate || (sym.hasAccessBoundary && !sym.isProtected)

  /** Checks that the optional argument to `@JSGlobal` on [[sym]] is a literal.
   *
   *  Reports an error on each annotation where this is not the case.
   */
  private def checkJSGlobalLiteral(sym: Symbol): Unit = {
    for {
      annot <- sym.getAnnotation(JSGlobalAnnotation)
      if annot.args.nonEmpty
    } {
      assert(annot.args.size == 1,
          s"@JSGlobal annotation $annot has more than 1 argument")

      val argIsValid = annot.stringArg(0).isDefined
      if (!argIsValid) {
        reporter.error(annot.args(0).pos,
            "The argument to @JSGlobal must be a literal string.")
      }
    }
  }

  /** Checks that arguments to `@JSImport` on [[sym]] are literals.
   *
   *  The second argument can also be the singleton `JSImport.Namespace`
   *  object.
   *
   *  Reports an error on each annotation where this is not the case.
   */
  private def checkJSImportLiteral(sym: Symbol): Unit = {
    for {
      annot <- sym.getAnnotation(JSImportAnnotation)
    } {
      assert(annot.args.size == 2 || annot.args.size == 3,
          s"@JSImport annotation $annot does not have exactly 2 or 3 arguments")

      val firstArgIsValid = annot.stringArg(0).isDefined
      if (!firstArgIsValid) {
        reporter.error(annot.args(0).pos,
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
      reporter.error(annot.pos, "@JSName annotations are not allowed on top " +
          "level classes or objects (or classes and objects inside Scala objects).")
    }

    val annots = sym.annotations.filter { annot =>
      JSNativeLoadingSpecAnnots.contains(annot.symbol)
    }

    val badAnnotCountMsg =
      "Native JS classes and objects must have exactly one " +
      "annotation among @JSGlobal, @JSImport and @JSGlobalScope."

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

  /** checks if the primary constructor of the ClassDef `cldef` does not
   *  take any arguments
   */
  private def primCtorNoArg(cldef: ClassDef) =
    getPrimCtor(cldef.symbol.tpe).map(_.paramss == List(List())).getOrElse(true)

  /** return the MethodSymbol of the primary constructor of the given type
   *  if it exists
   */
  private def getPrimCtor(tpe: Type) =
    tpe.declaration(nme.CONSTRUCTOR).alternatives.collectFirst {
      case ctor: MethodSymbol if ctor.isPrimaryConstructor => ctor
    }

  private def shouldModuleBeExposed(sym: Symbol) = {
    assert(sym.isModuleOrModuleClass, sym)
    !sym.isLocalToBlock && !sym.isSynthetic && !isPrivateMaybeWithin(sym)
  }

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

  private def checkInternalAnnotations(tree: Tree): Unit = {
    /** Returns true iff it is a compiler annotations. This does not include
     *  annotations inserted before the typer (such as `@WasPublicBeforeTyper`).
     */
    def isCompilerAnnotation(annotation: AnnotationInfo): Boolean = {
      annotation.symbol == ExposedJSMemberAnnot ||
      annotation.symbol == JSTypeAnnot ||
      annotation.symbol == JSOptionalAnnotation
    }

    if (tree.isInstanceOf[MemberDef]) {
      for (annotation <- tree.symbol.annotations) {
        if (isCompilerAnnotation(annotation)) {
          reporter.error(annotation.pos,
              s"$annotation is for compiler internal use only. " +
              "Do not use it yourself.")
        }
      }
    }
  }
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
