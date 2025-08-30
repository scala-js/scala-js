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

import scala.annotation.tailrec

import scala.collection.mutable

import org.scalajs.ir.{ClassKind, Position, Version}
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees.{JSNativeLoadSpec, MemberNamespace}
import org.scalajs.ir.WellKnownNames._

import org.scalajs.logging._

import org.scalajs.linker.interface._
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID
import org.scalajs.linker.backend.javascript.{Trees => js, _}
import org.scalajs.linker.CollectionsCompat.MutableMapCompatOps

import EmitterNames._
import GlobalRefUtils._

/** Emits a desugared JS tree to a builder */
final class Emitter(config: Emitter.Config, prePrinter: Emitter.PrePrinter) {

  import Emitter._
  import config._
  import coreSpec._

  require(!config.minify || prePrinter == PrePrinter.Off,
      "When using the 'minify' option, the prePrinter must be Off.")

  private implicit val globalRefTracking: GlobalRefTracking =
    config.topLevelGlobalRefTracking

  private val knowledgeGuardian = new KnowledgeGuardian(config)

  private val uncachedKnowledge = new knowledgeGuardian.KnowledgeAccessor {}

  private val nameGen: NameGen = new NameGen

  private class State(val lastMentionedDangerousGlobalRefs: Set[String]) {
    val nameCompressor =
      if (minify) Some(new NameCompressor(config))
      else None

    val sjsGen: SJSGen = {
      val jsGen = new JSGen(config)
      val varGen = new VarGen(jsGen, nameGen, lastMentionedDangerousGlobalRefs)
      new SJSGen(jsGen, nameGen, varGen, nameCompressor)
    }

    val classEmitter: ClassEmitter = new ClassEmitter(sjsGen)

    val everyFileStart: List[js.Tree] = {
      // This prePrint does not count in the statistics
      prePrinter.prePrint(sjsGen.declarePrototypeVar, 0)
    }

    val coreJSLibCache: CoreJSLibCache = new CoreJSLibCache

    val moduleCaches: mutable.Map[ModuleID, ModuleCache] = mutable.Map.empty

    val classCaches: mutable.Map[ClassID, ClassCache] = mutable.Map.empty
  }

  private var state: State = new State(Set.empty)

  private def jsGen: JSGen = state.sjsGen.jsGen
  private def sjsGen: SJSGen = state.sjsGen
  private def classEmitter: ClassEmitter = state.classEmitter
  private def classCaches: mutable.Map[ClassID, ClassCache] = state.classCaches

  private[this] var statsClassesReused: Int = 0
  private[this] var statsClassesInvalidated: Int = 0
  private[this] var statsMethodsReused: Int = 0
  private[this] var statsMethodsInvalidated: Int = 0
  private[this] var statsPrePrints: Int = 0

  val symbolRequirements: SymbolRequirement =
    Emitter.symbolRequirements(config)

  val injectedIRFiles: Seq[IRFile] = PrivateLibHolder.files

  def emit(moduleSet: ModuleSet, logger: Logger): Result = {
    val WithGlobals(body, globalRefs) = emitInternal(moduleSet, logger)

    val result = moduleKind match {
      case ModuleKind.NoModule =>
        assert(moduleSet.modules.size <= 1)
        val topLevelVars = moduleSet.modules
          .headOption.toList
          .flatMap(_.topLevelExports)
          .map(_.exportName)

        val header = {
          val maybeTopLevelVarDecls = if (topLevelVars.nonEmpty) {
            val kw = if (esFeatures.useECMAScript2015Semantics) "let " else "var "
            topLevelVars.mkString(kw, ",", ";\n")
          } else {
            ""
          }
          config.jsHeader + maybeTopLevelVarDecls + "(function(){\n"
        }

        val footer = "}).call(this);\n"

        new Result(header, body, footer, topLevelVars, globalRefs)

      case ModuleKind.ESModule | ModuleKind.CommonJSModule =>
        new Result(config.jsHeader, body, "", Nil, globalRefs)
    }

    for (compressor <- state.nameCompressor) {
      compressor.allocateNames(moduleSet, logger)

      /* Throw away the whole state, but keep the mentioned dangerous global refs.
       * Note that instances of the name compressor's entries are still alive
       * at this point, since they are referenced from `DelayedIdent` nodes in
       * the result trees.
       */
      state = new State(state.lastMentionedDangerousGlobalRefs)
    }

    result
  }

  private def emitInternal(moduleSet: ModuleSet,
      logger: Logger): WithGlobals[Map[ModuleID, (List[js.Tree], Boolean)]] = {
    // Reset caching stats.
    statsClassesReused = 0
    statsClassesInvalidated = 0
    statsMethodsReused = 0
    statsMethodsInvalidated = 0
    statsPrePrints = 0

    // Update GlobalKnowledge.
    val invalidateAll = knowledgeGuardian.update(moduleSet)
    if (invalidateAll) {
      state.coreJSLibCache.invalidate()
      classCaches.clear()
    }

    // Inform caches about new run.
    classCaches.valuesIterator.foreach(_.startRun())

    try {
      emitAvoidGlobalClash(moduleSet, logger, secondAttempt = false)
    } finally {
      // Report caching stats (extracted in EmitterTest).
      logger.debug(
          s"Emitter: Class tree cache stats: reused: $statsClassesReused -- "+
          s"invalidated: $statsClassesInvalidated")
      logger.debug(
          s"Emitter: Method tree cache stats: reused: $statsMethodsReused -- "+
          s"invalidated: $statsMethodsInvalidated")
      logger.debug(s"Emitter: Pre prints: $statsPrePrints")

      // Inform caches about run completion.
      state.moduleCaches.filterInPlace((_, c) => c.cleanAfterRun())
      classCaches.filterInPlace((_, c) => c.cleanAfterRun())
    }
  }

  private def prePrint(trees: List[js.Tree], indent: Int): List[js.Tree] = {
    statsPrePrints += 1
    prePrinter.prePrint(trees, indent)
  }

  private def prePrint(tree: js.Tree, indent: Int): List[js.Tree] =
    prePrint(tree :: Nil, indent)

  /** Emits all JavaScript code avoiding clashes with global refs.
   *
   *  If, at the end of the process, the set of accessed dangerous globals has
   *  changed, invalidate *everything* and start over. If at first you don't
   *  succeed, ...
   */
  @tailrec
  private def emitAvoidGlobalClash(moduleSet: ModuleSet,
      logger: Logger, secondAttempt: Boolean): WithGlobals[Map[ModuleID, (List[js.Tree], Boolean)]] = {
    val result = emitOnce(moduleSet, logger)

    val mentionedDangerousGlobalRefs =
      GlobalRefTracking.Dangerous.refineFrom(topLevelGlobalRefTracking, result.globalVarNames)

    if (mentionedDangerousGlobalRefs == state.lastMentionedDangerousGlobalRefs) {
      result
    } else {
      assert(!secondAttempt,
          "Uh oh! The second attempt gave a different set of dangerous " +
          "global refs than the first one.\n" +
          "Before:" + state.lastMentionedDangerousGlobalRefs.toList.sorted.mkString("\n  ", "\n  ", "\n") +
          "After:" + mentionedDangerousGlobalRefs.toList.sorted.mkString("\n  ", "\n  ", ""))

      // !!! This log message is tested in EmitterTest
      logger.debug(
          "Emitter: The set of dangerous global refs has changed. " +
          "Going to re-generate the world.")

      state = new State(mentionedDangerousGlobalRefs)
      emitAvoidGlobalClash(moduleSet, logger, secondAttempt = true)
    }
  }

  private def emitOnce(moduleSet: ModuleSet,
      logger: Logger): WithGlobals[Map[ModuleID, (List[js.Tree], Boolean)]] = {
    // Genreate classes first so we can measure time separately.
    val generatedClasses = logger.time("Emitter: Generate Classes") {
      moduleSet.modules.map { module =>
        val moduleContext = ModuleContext.fromModule(module)
        val orderedClasses = module.classDefs.sortWith(compareClasses)
        module.id -> orderedClasses.map(genClass(_, moduleContext))
      }.toMap
    }

    var trackedGlobalRefs = Set.empty[String]
    def extractWithGlobals[T](x: WithGlobals[T]) = {
      trackedGlobalRefs = unionPreserveEmpty(trackedGlobalRefs, x.globalVarNames)
      x.value
    }

    val moduleTrees = logger.time("Emitter: Write trees") {
      moduleSet.modules.map { module =>
        var changed = false
        def extractChangedAndWithGlobals[T](x: (WithGlobals[T], Boolean)): T = {
          changed ||= x._2
          extractWithGlobals(x._1)
        }

        val moduleContext = ModuleContext.fromModule(module)
        val moduleCache = state.moduleCaches.getOrElseUpdate(module.id, new ModuleCache)

        val moduleClasses = generatedClasses(module.id)

        changed ||= moduleClasses.exists(_.changed)

        val moduleImports = extractChangedAndWithGlobals {
          moduleCache.getOrComputeImports(module.externalDependencies, module.internalDependencies) {
            genModuleImports(module).map(prePrint(_, 0))
          }
        }

        val topLevelExports = extractChangedAndWithGlobals {
          /* We cache top level exports all together, rather than individually,
           * since typically there are few.
           */
          moduleCache.getOrComputeTopLevelExports(module.topLevelExports) {
            classEmitter.genTopLevelExports(module.topLevelExports)(
                moduleContext, moduleCache).map(prePrint(_, 0))
          }
        }

        val moduleInitializers = extractChangedAndWithGlobals {
          val initializers = module.initializers.toList
          moduleCache.getOrComputeInitializers(initializers) {
            WithGlobals.list(initializers.map { initializer =>
              classEmitter.genModuleInitializer(initializer)(
                  moduleContext, moduleCache)
            }).map(prePrint(_, 0))
          }
        }

        val coreJSLib =
          if (module.isRoot) Some(extractWithGlobals(state.coreJSLibCache.build(moduleContext)))
          else None

        def classIter = moduleClasses.iterator

        def objectClass =
          if (!module.isRoot) Iterator.empty
          else classIter.filter(_.className == ObjectClass)

        /* Emit everything but module imports in the appropriate order.
         *
         * We do not emit module imports to be able to assert that the
         * resulting module is non-empty. This is a non-trivial condition that
         * requires consistency between the Analyzer and the Emitter. As such,
         * it is crucial that we verify it.
         */
        val defTrees: List[js.Tree] = (
            /* The declaration of the `$p` variable that temporarily holds
             * prototypes.
             */
            state.everyFileStart.iterator ++

            /* The definitions of the CoreJSLib that come before the definition
             * of `j.l.Object`. They depend on nothing else.
             */
            coreJSLib.iterator.flatMap(_.preObjectDefinitions) ++

            /* The definition of `j.l.Object` class. Unlike other classes, this
             * does not include its instance tests nor metadata.
             */
            objectClass.flatMap(_.main) ++

            /* The definitions of the CoreJSLib that come after the definition
             * of `j.l.Object` because they depend on it. This includes the
             * definitions of the array classes, as well as type data for
             * primitive types and for `j.l.Object`.
             */
            coreJSLib.iterator.flatMap(_.postObjectDefinitions) ++

            /* All class definitions, except `j.l.Object`, which depend on
             * nothing but their superclasses.
             */
            classIter.filterNot(_.className == ObjectClass).flatMap(_.main) ++

            /* The initialization of the CoreJSLib, which depends on the
             * definition of classes (n.b. the RuntimeLong class).
             */
            coreJSLib.iterator.flatMap(_.initialization) ++

            /* All static field definitions, which depend on nothing, except
             * those of type Long which need $L0.
             */
            classIter.flatMap(_.staticFields) ++

            /* All static initializers, which in the worst case can observe some
             * "zero" state of other static field definitions, but must not
             * observe a *non-initialized* (undefined) state.
             */
            classIter.flatMap(_.staticInitialization) ++

            /* All the exports, during which some JS class creation can happen,
             * causing JS static initializers to run. Those also must not observe
             * a non-initialized state of other static fields.
             */
            topLevelExports.iterator ++

            /* Module initializers, which by spec run at the end. */
            moduleInitializers.iterator
        ).toList

        // Make sure that there is at least one non-import definition.
        assert(!defTrees.isEmpty, {
            val classNames = module.classDefs.map(_.fullName).mkString(", ")
            s"Module ${module.id} is empty. Classes in this module: $classNames"
        })

        /* Add module imports, which depend on nothing, at the front.
         * All classes potentially depend on them.
         */
        val allTrees = moduleImports ::: defTrees

        classIter.foreach { genClass =>
          trackedGlobalRefs = unionPreserveEmpty(trackedGlobalRefs, genClass.trackedGlobalRefs)
        }

        module.id -> (allTrees, changed)
      }
    }

    WithGlobals(moduleTrees.toMap, trackedGlobalRefs)
  }

  private def genModuleImports(module: ModuleSet.Module): WithGlobals[List[js.Tree]] = {
    implicit val pos = Position.NoPosition

    def importParts = (
        (
            module.externalDependencies.map { x =>
              sjsGen.varGen.externalModuleFieldIdent(x) -> x
            }
        ) ++ (
            module.internalDependencies.map { x =>
              sjsGen.varGen.internalModuleFieldIdent(x) -> config.internalModulePattern(x)
            }
        )
    ).toList.sortBy(_._1.name)

    moduleKind match {
      case ModuleKind.NoModule =>
        WithGlobals.nil

      case ModuleKind.ESModule =>
        val imports = importParts.map { case (ident, moduleName) =>
          val from = js.StringLiteral(moduleName)
          js.ImportNamespace(ident, from)
        }
        WithGlobals(imports)

      case ModuleKind.CommonJSModule =>
        val imports = importParts.map { case (ident, moduleName) =>
          for (requireRef <- jsGen.globalRef("require")) yield {
            val rhs = js.Apply(requireRef, List(js.StringLiteral(moduleName)))
            jsGen.genLet(ident, mutable = false, rhs)
          }
        }
        WithGlobals.list(imports)
    }
  }

  private def compareClasses(lhs: LinkedClass, rhs: LinkedClass) = {
    val lhsAC = lhs.ancestors.size
    val rhsAC = rhs.ancestors.size
    if (lhsAC != rhsAC) lhsAC < rhsAC
    else lhs.className.compareTo(rhs.className) < 0
  }

  private def genClass(
      linkedClass_! : LinkedClass, // scalastyle:ignore
      moduleContext: ModuleContext): GeneratedClass = {

    /* !!! In this method, *every* time you use linkedClass_!, you must justify
     * why you have the right to access the fields you are reading.
     * That's why we give it that dangerous-looking name.
     */

    // Cache identity; always safe to access
    val kind = linkedClass_!.kind
    val isJSClass = kind.isJSClass // we use this one a lot
    val className = linkedClass_!.className
    val ancestors = linkedClass_!.ancestors

    val classCache = classCaches.getOrElseUpdate(
        new ClassID(kind, ancestors, moduleContext), new ClassCache)

    var changed = false
    def extractChanged[T](x: (T, Boolean)): T = {
      changed ||= x._2
      x._1
    }

    /* The class version itself; it's OK to get that one as long as we don't
     * use it to *produce* trees, which we should never do anyway.
     */
    val classVersion = linkedClass_!.version

    val classTreeCache = extractChanged(classCache.getCache(classVersion))

    /* Information that we can use for uncached decision-making.
     * We call "decision-making" any if/else branch not covered by an explicit
     * cache.
     *
     * Note that it is *not* safe to use uncachedDecisions in *cached*
     * decision-making!
     */
    val uncachedDecisions = extractChanged(classCache.getUncachedDecisions(linkedClass_!))

    /* Delegate justifications for those two to their use sites.
     *
     * - Uses of linkedInlineableInit_! must be protected by the classVersion
     *   and its own version (see `ctorVersion`).
     * - The sets of things generated by linkedMethods_! must be independently
     *   tracked (see `trackStaticLikeMethodChanges` and `fullClassChangeTracker`).
     */
    val (linkedInlineableInit_!, linkedMethods_!) =
      classEmitter.extractInlineableInit(linkedClass_!)(classCache)

    // Global ref management

    var trackedGlobalRefs: Set[String] = Set.empty

    def extractWithGlobals[T](withGlobals: WithGlobals[T]): T = {
      trackedGlobalRefs = unionPreserveEmpty(trackedGlobalRefs, withGlobals.globalVarNames)
      withGlobals.value
    }

    def extractWithGlobalsAndChanged[T](x: (WithGlobals[T], Boolean)): T =
      extractWithGlobals(extractChanged(x))

    // Main part

    val main = List.newBuilder[js.Tree]

    // Symbols for private JS fields
    if (isJSClass) {
      val fieldDefs = classTreeCache.privateJSFields.getOrElseUpdate {
        classEmitter.genCreatePrivateJSFieldDefsOfJSClass(className)(
            moduleContext, classCache).map(prePrint(_, 0))
      }
      main ++= extractWithGlobals(fieldDefs)
    }

    // Static-like methods
    locally {
      val emitAllAsStaticLike =
        kind == ClassKind.Interface || kind == ClassKind.HijackedClass

      // The set is tracked explicitly just after
      val staticLikeMethods = for {
        methodDef <- linkedMethods_! // versioning per member inside
        if emitAllAsStaticLike || methodDef.flags.namespace != MemberNamespace.Public
      } yield {
        val methodCache =
          classCache.getStaticLikeMethodCache(methodDef.flags.namespace, methodDef.methodName)

        extractWithGlobalsAndChanged(methodCache.getOrElseUpdate(methodDef.version, {
          classEmitter.genStaticLikeMethod(className, methodDef)(moduleContext, methodCache)
            .map(prePrint(_, 0))
        }))
      }

      // *Non* short-circuiting boolean or; always evaluate the right-hand-side
      changed |= classCache.trackStaticLikeMethodChanges(staticLikeMethods)

      for (staticLikeMethod <- staticLikeMethods)
        main ++= staticLikeMethod
    }

    // Class definition
    if (uncachedDecisions.hasInstances && kind.isAnyNonNativeClass) {
      /* Decision-making in this scope is governed by the `fullClassChangeTracker`
       * below *instead* of the uncachedDecisions. That means we are not allowed
       * to use `uncachedDecisions` in this scope.
       *
       * The `fullClassChangeTracker` is actually called at the end of the
       * block, because it needs to track *produced* js.Trees in addition to
       * *inputs* in the LinkedClass. This way, it essentially tracks the *set*
       * of methods that are emitted (i.e., that were not dce'ed away before
       * getting here).
       *
       * Nevertheless, since it tracks the class version, we are allowed to
       * use the following facts for uncached decision-making.
       */

      val hasJSSuperClass = linkedClass_!.jsSuperClass.isDefined

      /* Whether or not there is a class initializer is a constant across the
       * pipeline. By construction, if there is one in the original `ClassDef`,
       * it must still be here now. The `classVersion` covers the *entire*
       * original `ClassDef` (though it does not cover the entire
       * `LinkedClass`es after transformations). So the `classVersion` covers
       * whether or not there is a class initializer.
       */
      val hasClassInitializer: Boolean = {
        linkedClass_!.methods.exists { m =>
          m.flags.namespace == MemberNamespace.StaticConstructor &&
          m.methodName.isClassInitializer
        }
      }

      /* Is this class compiled as an ECMAScript `class`?
       *
       * See JSGen.useClassesForRegularClasses for the rationale here.
       *
       * This value is a "constant-per-cache", because it only depends on
       * - the global linker config, and
       * - isJSClass/ancestors, which are always safe (part of the cache identity).
       *
       * Note that `useClassesForRegularClasses` implies
       * `useClassesForJSClassesAndThrowables`, so the short-cut is valid.
       *
       * Compared to `ClassEmitter.shouldExtendJSError`, which is used below,
       * we do not check here that `Throwable` directly extends `Object`. If
       * that is not the case (for some obscure reason), then we are going to
       * uselessly emit `class`es for Throwables, but that will not make any
       * observable change; whereas rewiring Throwable to extend `Error` when
       * it does not actually directly extend `Object` would break everything,
       * so we need to be more careful there.
       */
      val useESClass = if (jsGen.useClassesForRegularClasses) {
        assert(jsGen.useClassesForJSClassesAndThrowables)
        true
      } else {
        jsGen.useClassesForJSClassesAndThrowables &&
        (isJSClass || ancestors.contains(ThrowableClass))
      }

      // Therefore, this is also constant-per-cache
      val memberIndent = {
        (if (isJSClass) 1 else 0) + // accessor function
        (if (useESClass) 1 else 0) // nesting from class
      }

      val storeJSSuperClass = if (hasJSSuperClass) {
        extractWithGlobals(classTreeCache.storeJSSuperClass.getOrElseUpdate({
          // jsSuperClass and pos invalidated by class version
          val jsSuperClass = linkedClass_!.jsSuperClass.get
          classEmitter.genStoreJSSuperClass(jsSuperClass)(moduleContext, classCache, linkedClass_!.pos)
            .map(prePrint(_, 1))
        }))
      } else {
        Nil
      }

      // JS constructor
      val ctorWithGlobals = extractChanged {
        /* The constructor depends both on the class version, and the version
         * of the inlineable init, if there is one.
         *
         * If it is a JS class, it depends on the jsConstructorDef.
         */
        val ctorCache = classCache.getConstructorCache()

        if (isJSClass) {
          assert(linkedInlineableInit_!.isEmpty) // just an assertion, it's fine

          // Explicitly versioned just below
          val jsConstructorDef = linkedClass_!.jsConstructorDef.getOrElse {
            throw new IllegalArgumentException(s"$className does not have an exported constructor")
          }

          val ctorVersion = Version.combine(classVersion, jsConstructorDef.version)
          ctorCache.getOrElseUpdate(ctorVersion,
              classEmitter.genJSConstructor(
                className, // always safe
                linkedClass_!.superClass, // invalidated by class version
                linkedClass_!.jsSuperClass.isDefined, // invalidated by class version (*not* local decision-making)
                useESClass, // always safe
                jsConstructorDef // part of ctor version
              )(moduleContext, ctorCache, linkedClass_!.pos) // pos invalidated by class version
                .map(prePrint(_, memberIndent))
          )
        } else {
          val ctorVersion = linkedInlineableInit_!.fold { // versioning
            Version.combine(classVersion)
          } { linkedInit =>
            Version.combine(classVersion, linkedInit.version)
          }

          ctorCache.getOrElseUpdate(ctorVersion,
              classEmitter.genScalaClassConstructor(
                className, // always safe
                linkedClass_!.superClass, // invalidated by class version
                useESClass, // invalidated by class version,
                linkedInlineableInit_! // part of ctor version
              )(moduleContext, ctorCache, linkedClass_!.pos) // pos invalidated by class version
                .map(prePrint(_, memberIndent))
          )
        }
      }

      /* Bridges from Throwable to methods of Object, which are necessary
       * because Throwable is rewired to extend JavaScript's Error instead of
       * j.l.Object.
       *
       * Completely uncached; delegate justifications to use sites.
       */
      val linkedMethodsAndBridges_! = if (ClassEmitter.shouldExtendJSError(className)) {
        val existingMethods_! = linkedMethods_!
          .withFilter(_.flags.namespace == MemberNamespace.Public)
          .map(_.methodName)
          .toSet

        val bridges_! = for {
          methodDef <- uncachedKnowledge.methodsInObject()
          if !existingMethods_!.contains(methodDef.methodName)
        } yield {
          import org.scalajs.ir.Trees._
          import org.scalajs.ir.Types._

          implicit val pos = methodDef.pos

          val methodName = methodDef.name
          val newBody = ApplyStatically(ApplyFlags.empty,
              This()(ClassType(className, nullable = false)),
              ObjectClass, methodName, methodDef.args.map(_.ref))(
              methodDef.resultType)
          MethodDef(MemberFlags.empty, methodName,
              methodDef.originalName, methodDef.args, methodDef.resultType,
              Some(newBody))(
              OptimizerHints.empty, methodDef.version)
        }

        linkedMethods_! ++ bridges_!
      } else {
        linkedMethods_!
      }

      // Normal methods -- the set itself is tracked by the fullClassChangeTracker
      val memberMethodsWithGlobals = for {
        method <- linkedMethodsAndBridges_! // explicitly versioned below, by member
        if method.flags.namespace == MemberNamespace.Public
      } yield {
        val methodCache =
          classCache.getMemberMethodCache(method.methodName)

        extractChanged(methodCache.getOrElseUpdate(method.version,
            classEmitter.genMemberMethod(
                className, // always safe
                isJSClass, // always safe
                useESClass, // always safe
                method // invalidated by method.version
            )(moduleContext, methodCache).map(prePrint(_, memberIndent))))
      }

      // Exported Members -- the set itself is tracked by the fullClassChangeTracker
      val exportedMembersWithGlobals = for {
        (member, idx) <- linkedClass_!.exportedMembers.zipWithIndex // explicitly versioned below, by member
      } yield {
        val memberCache = classCache.getExportedMemberCache(idx)
        extractChanged(memberCache.getOrElseUpdate(member.version,
            classEmitter.genExportedMember(
                className, // always safe
                isJSClass, // always safe
                useESClass, // always safe
                member // invalidated by member.version
            )(moduleContext, memberCache).map(prePrint(_, memberIndent))))
      }

      // Now that we have everything, track changes.
      val fullClassChangeTracker = classCache.getFullClassChangeTracker()
      // Put changed state into a val to avoid short circuiting behavior of ||.
      val classChanged = fullClassChangeTracker.trackChanged(
          classVersion, ctorWithGlobals,
          memberMethodsWithGlobals, exportedMembersWithGlobals)
      changed ||= classChanged

      val fullClass = {
        for {
          ctor <- ctorWithGlobals
          memberMethods <- WithGlobals.flatten(memberMethodsWithGlobals)
          exportedMembers <- WithGlobals.flatten(exportedMembersWithGlobals)
          allMembers = ctor ::: memberMethods ::: exportedMembers
          clazz <- classEmitter.buildClass(
            className, // always safe
            isJSClass, // always safe
            linkedClass_!.jsClassCaptures, // invalidated by class version
            hasClassInitializer, // scope-local decision-making
            linkedClass_!.superClass, // invalidated by class version
            storeJSSuperClass, // invalidated by class version
            useESClass, // always safe
            allMembers // invalidated directly
          )(moduleContext, fullClassChangeTracker, linkedClass_!.pos) // pos invalidated by class version
        } yield {
          clazz
        }
      }

      main ++= extractWithGlobals(fullClass)
    }

    if (className != ObjectClass) {
      /* Instance tests and type data are hardcoded in the CoreJSLib for
       * j.l.Object. This is important because their definitions depend on the
       * `$TypeData` definition, which only comes in the `postObjectDefinitions`
       * of the CoreJSLib. If we wanted to define them here as part of the
       * normal logic of `ClassEmitter`, we would have to further divide `main`
       * into two parts. Since the code paths are in fact completely different
       * for `j.l.Object` anyway, we do not do this, and instead hard-code them
       * in the CoreJSLib. This explains why we exclude `j.l.Object` as this
       * level, rather than inside `ClassEmitter.needInstanceTests` and
       * similar: it is a concern that goes beyond the organization of the
       * class `j.l.Object`.
       */

      if (uncachedDecisions.needInstanceTests) {
        main ++= extractWithGlobals(classTreeCache.instanceTests.getOrElseUpdate({
          // pos invalidated by class version
          classEmitter.genInstanceTests(className, kind)(moduleContext, classCache, linkedClass_!.pos)
            .map(prePrint(_, 0))
        }))
      }

      if (uncachedDecisions.hasRuntimeTypeInfo) {
        main ++= extractWithGlobals(classTreeCache.typeData.getOrElseUpdate(
            linkedClass_!.hasDirectInstances, // versioning
            classEmitter.genTypeData(
              className, // always safe
              kind, // always safe
              linkedClass_!.superClass, // invalidated by class version
              ancestors, // always safe
              linkedClass_!.jsNativeLoadSpec, // invalidated by class version
              linkedClass_!.hasDirectInstances // invalidated directly (it is the input to `getOrElseUpdate`)
            )(moduleContext, classCache, linkedClass_!.pos).map(prePrint(_, 0)))) // pos invalidated by class version
      }
    }

    if (kind.hasModuleAccessor && uncachedDecisions.hasInstances) {
      main ++= extractWithGlobals(classTreeCache.moduleAccessor.getOrElseUpdate({
        // pos invalidated by class version
        classEmitter.genModuleAccessor(className, isJSClass)(moduleContext, classCache, linkedClass_!.pos)
          .map(prePrint(_, 0))
      }))
    }

    // Static fields

    val staticFields = if (kind.isJSType) {
      Nil
    } else {
      extractWithGlobals(classTreeCache.staticFields.getOrElseUpdate({
        classEmitter.genCreateStaticFieldsOfScalaClass(className)(moduleContext, classCache)
          .map(prePrint(_, 0))
      }))
    }

    // Static initialization

    val staticInitialization = if (uncachedDecisions.needStaticInitialization) {
      classTreeCache.staticInitialization.getOrElseUpdate({
        // pos invalidated by class version
        val tree = classEmitter.genStaticInitialization(className)(moduleContext, classCache, linkedClass_!.pos)
        prePrint(tree, 0)
      })
    } else {
      Nil
    }

    // Build the result

    new GeneratedClass(
        className,
        main.result(),
        staticFields,
        staticInitialization,
        trackedGlobalRefs,
        changed
    )
  }

  // Caching

  private final class ModuleCache extends knowledgeGuardian.KnowledgeAccessor {
    private[this] var _cacheUsed: Boolean = false

    private[this] var _importsCache: WithGlobals[List[js.Tree]] = WithGlobals.nil
    private[this] var _lastExternalDependencies: Set[String] = Set.empty
    private[this] var _lastInternalDependencies: Set[ModuleID] = Set.empty

    private[this] var _topLevelExportsCache: WithGlobals[List[js.Tree]] = WithGlobals.nil
    private[this] var _lastTopLevelExports: List[LinkedTopLevelExport] = Nil

    private[this] var _initializersCache: WithGlobals[List[js.Tree]] = WithGlobals.nil
    private[this] var _lastInitializers: List[ModuleInitializer.Initializer] = Nil

    override def invalidate(): Unit = {
      super.invalidate()

      /* In order to keep reasoning as local as possible, we also invalidate
       * the imports cache, although imports do not use any global knowledge.
       */
      _importsCache = WithGlobals.nil
      _lastExternalDependencies = Set.empty
      _lastInternalDependencies = Set.empty

      _topLevelExportsCache = WithGlobals.nil
      _lastTopLevelExports = Nil

      _initializersCache = WithGlobals.nil
      _lastInitializers = Nil
    }

    def getOrComputeImports(externalDependencies: Set[String], internalDependencies: Set[ModuleID])(
        compute: => WithGlobals[List[js.Tree]]): (WithGlobals[List[js.Tree]], Boolean) = {

      _cacheUsed = true

      if (externalDependencies != _lastExternalDependencies || internalDependencies != _lastInternalDependencies) {
        _importsCache = compute
        _lastExternalDependencies = externalDependencies
        _lastInternalDependencies = internalDependencies
        (_importsCache, true)
      } else {
        (_importsCache, false)
      }

    }

    def getOrComputeTopLevelExports(topLevelExports: List[LinkedTopLevelExport])(
        compute: => WithGlobals[List[js.Tree]]): (WithGlobals[List[js.Tree]], Boolean) = {

      _cacheUsed = true

      if (!sameTopLevelExports(topLevelExports, _lastTopLevelExports)) {
        _topLevelExportsCache = compute
        _lastTopLevelExports = topLevelExports
        (_topLevelExportsCache, true)
      } else {
        (_topLevelExportsCache, false)
      }
    }

    private def sameTopLevelExports(tles1: List[LinkedTopLevelExport], tles2: List[LinkedTopLevelExport]): Boolean = {
      import org.scalajs.ir.Trees._

      /* Because of how/when we use this method, we already know that all the
       * `tles1` and `tles2` have the same `moduleID` (namely the ID of the
       * module represented by this `ModuleCache`). Therefore, we do not
       * compare that field.
       */

      tles1.corresponds(tles2) { (tle1, tle2) =>
        tle1.tree.pos == tle2.tree.pos && tle1.owningClass == tle2.owningClass && {
          (tle1.tree, tle2.tree) match {
            case (TopLevelJSClassExportDef(_, exportName1), TopLevelJSClassExportDef(_, exportName2)) =>
              exportName1 == exportName2
            case (TopLevelModuleExportDef(_, exportName1), TopLevelModuleExportDef(_, exportName2)) =>
              exportName1 == exportName2
            case (TopLevelMethodExportDef(_, methodDef1), TopLevelMethodExportDef(_, methodDef2)) =>
              methodDef1.version.sameVersion(methodDef2.version)
            case (TopLevelFieldExportDef(_, exportName1, field1), TopLevelFieldExportDef(_, exportName2, field2)) =>
              exportName1 == exportName2 && field1.name == field2.name && field1.pos == field2.pos
            case _ =>
              false
          }
        }
      }
    }

    def getOrComputeInitializers(initializers: List[ModuleInitializer.Initializer])(
        compute: => WithGlobals[List[js.Tree]]): (WithGlobals[List[js.Tree]], Boolean) = {

      _cacheUsed = true

      if (initializers != _lastInitializers) {
        _initializersCache = compute
        _lastInitializers = initializers
        (_initializersCache, true)
      } else {
        (_initializersCache, false)
      }
    }

    def cleanAfterRun(): Boolean = {
      val result = _cacheUsed
      _cacheUsed = false
      result
    }
  }

  private final class ClassCache extends knowledgeGuardian.KnowledgeAccessor {
    private[this] var _cache: DesugaredClassCache = null
    private[this] var _lastVersion: Version = Version.Unversioned
    private[this] var _cacheUsed = false

    private[this] var _uncachedDecisions: UncachedDecisions = UncachedDecisions.Invalid

    private[this] val _methodCaches =
      Array.fill(MemberNamespace.Count)(mutable.Map.empty[MethodName, MethodCache[List[js.Tree]]])

    private[this] val _memberMethodCache =
      mutable.Map.empty[MethodName, MethodCache[List[js.Tree]]]

    private[this] var _constructorCache: Option[MethodCache[List[js.Tree]]] = None

    private[this] val _exportedMembersCache =
      mutable.Map.empty[Int, MethodCache[List[js.Tree]]]

    private[this] var _staticLikeMethodsTracker: Option[List[List[js.Tree]]] = None
    private[this] var _fullClassChangeTracker: Option[FullClassChangeTracker] = None

    override def invalidate(): Unit = {
      /* Do not invalidate contained methods, as they have their own
       * invalidation logic.
       */
      super.invalidate()
      _cache = null
      _lastVersion = Version.Unversioned
      _uncachedDecisions = UncachedDecisions.Invalid
    }

    def startRun(): Unit = {
      _cacheUsed = false
      _methodCaches.foreach(_.valuesIterator.foreach(_.startRun()))
      _memberMethodCache.valuesIterator.foreach(_.startRun())
      _constructorCache.foreach(_.startRun())
      _fullClassChangeTracker.foreach(_.startRun())
    }

    def getCache(version: Version): (DesugaredClassCache, Boolean) = {
      _cacheUsed = true
      if (_cache == null || !_lastVersion.sameVersion(version)) {
        invalidate()
        statsClassesInvalidated += 1
        _lastVersion = version
        _cache = new DesugaredClassCache
        (_cache, true)
      } else {
        statsClassesReused += 1
        (_cache, false)
      }
    }

    def getUncachedDecisions(linkedClass: LinkedClass): (UncachedDecisions, Boolean) = {
      val needInstanceTests = classEmitter.needInstanceTests(linkedClass)(this)
      val needStaticInitialization = classEmitter.needStaticInitialization(linkedClass)
      val result = UncachedDecisions(
        hasInstances = linkedClass.hasInstances,
        hasRuntimeTypeInfo = linkedClass.hasRuntimeTypeInfo,
        hasInstanceTests = linkedClass.hasInstanceTests,
        needInstanceTests = needInstanceTests,
        needStaticInitialization = needStaticInitialization
      )
      val changed = result != _uncachedDecisions
      if (changed)
        _uncachedDecisions = result
      (result, changed)
    }

    def getMemberMethodCache(
        methodName: MethodName): MethodCache[List[js.Tree]] = {
      _memberMethodCache.getOrElseUpdate(methodName, new MethodCache)
    }

    def getStaticLikeMethodCache(namespace: MemberNamespace,
        methodName: MethodName): MethodCache[List[js.Tree]] = {
      _methodCaches(namespace.ordinal)
        .getOrElseUpdate(methodName, new MethodCache)
    }

    def getConstructorCache(): MethodCache[List[js.Tree]] = {
      _constructorCache.getOrElse {
        val cache = new MethodCache[List[js.Tree]]
        _constructorCache = Some(cache)
        cache
      }
    }

    def getExportedMemberCache(idx: Int): MethodCache[List[js.Tree]] =
      _exportedMembersCache.getOrElseUpdate(idx, new MethodCache)

    /** Track changes to the generated list of static-like methods.
     *
     *  Returns `true` iff there were changes since the last run.
     */
    def trackStaticLikeMethodChanges(staticLikeMethods: List[List[js.Tree]]): Boolean = {
      if (_staticLikeMethodsTracker.exists(allSame(_, staticLikeMethods))) {
        false
      } else {
        _staticLikeMethodsTracker = Some(staticLikeMethods)
        true
      }
    }

    def getFullClassChangeTracker(): FullClassChangeTracker = {
      _fullClassChangeTracker.getOrElse {
        val cache = new FullClassChangeTracker
        _fullClassChangeTracker = Some(cache)
        cache
      }
    }

    def cleanAfterRun(): Boolean = {
      _methodCaches.foreach(_.filterInPlace((_, c) => c.cleanAfterRun()))
      _memberMethodCache.filterInPlace((_, c) => c.cleanAfterRun())

      if (_constructorCache.exists(!_.cleanAfterRun()))
        _constructorCache = None

      _exportedMembersCache.filterInPlace((_, c) => c.cleanAfterRun())

      if (_fullClassChangeTracker.exists(!_.cleanAfterRun()))
        _fullClassChangeTracker = None

      if (!_cacheUsed)
        invalidate()

      _methodCaches.exists(_.nonEmpty) || _cacheUsed
    }
  }

  private final class MethodCache[T] extends knowledgeGuardian.KnowledgeAccessor {
    private[this] var _tree: WithGlobals[T] = null
    private[this] var _lastVersion: Version = Version.Unversioned
    private[this] var _cacheUsed = false

    override def invalidate(): Unit = {
      super.invalidate()
      _tree = null
      _lastVersion = Version.Unversioned
    }

    def startRun(): Unit = _cacheUsed = false

    def getOrElseUpdate(version: Version,
        v: => WithGlobals[T]): (WithGlobals[T], Boolean) = {
      _cacheUsed = true
      if (_tree == null || !_lastVersion.sameVersion(version)) {
        invalidate()
        statsMethodsInvalidated += 1
        _tree = v
        _lastVersion = version
        (_tree, true)
      } else {
        statsMethodsReused += 1
        (_tree, false)
      }
    }

    def cleanAfterRun(): Boolean = {
      if (!_cacheUsed)
        invalidate()

      _cacheUsed
    }
  }

  private class FullClassChangeTracker extends knowledgeGuardian.KnowledgeAccessor {
    private[this] var _lastVersion: Version = Version.Unversioned
    private[this] var _lastCtor: WithGlobals[List[js.Tree]] = null
    private[this] var _lastMemberMethods: List[WithGlobals[List[js.Tree]]] = null
    private[this] var _lastExportedMembers: List[WithGlobals[List[js.Tree]]] = null
    private[this] var _trackerUsed = false

    override def invalidate(): Unit = {
      super.invalidate()
      _lastVersion = Version.Unversioned
      _lastCtor = null
      _lastMemberMethods = null
      _lastExportedMembers = null
    }

    def startRun(): Unit = _trackerUsed = false

    def trackChanged(version: Version, ctor: WithGlobals[List[js.Tree]],
        memberMethods: List[WithGlobals[List[js.Tree]]],
        exportedMembers: List[WithGlobals[List[js.Tree]]]): Boolean = {

      _trackerUsed = true

      val changed = {
        !version.sameVersion(_lastVersion) ||
        (_lastCtor ne ctor) ||
        !allSame(_lastMemberMethods, memberMethods) ||
        !allSame(_lastExportedMembers, exportedMembers)
      }

      if (changed) {
        // Input has changed or we were invalidated.
        // Clean knowledge tracking and re-track dependencies.
        invalidate()
        _lastVersion = version
        _lastCtor = ctor
        _lastMemberMethods = memberMethods
        _lastExportedMembers = exportedMembers
      }

      changed
    }

    def cleanAfterRun(): Boolean = {
      if (!_trackerUsed)
        invalidate()

      _trackerUsed
    }
  }

  private class CoreJSLibCache extends knowledgeGuardian.KnowledgeAccessor {
    private[this] var _lastModuleContext: ModuleContext = _
    private[this] var _lib: WithGlobals[CoreJSLib.Lib[List[js.Tree]]] = _

    def build(moduleContext: ModuleContext): WithGlobals[CoreJSLib.Lib[List[js.Tree]]] = {
      if (_lib == null || _lastModuleContext != moduleContext) {
        _lib = CoreJSLib.build(sjsGen, prePrint(_, 0), moduleContext, this)
        _lastModuleContext = moduleContext
      }
      _lib
    }

    override def invalidate(): Unit = {
      super.invalidate()
      _lib = null
    }
  }
}

object Emitter {
  /** Result of an emitter run. */
  final class Result private[Emitter](
      val header: String,
      val body: Map[ModuleID, (List[js.Tree], Boolean)],
      val footer: String,
      val topLevelVarDecls: List[String],
      val globalRefs: Set[String]
  )

  /** Configuration for the Emitter. */
  final class Config private (
      val coreSpec: CoreSpec,
      val jsHeader: String,
      val internalModulePattern: ModuleID => String,
      val optimizeBracketSelects: Boolean,
      val trackAllGlobalRefs: Boolean,
      val minify: Boolean
  ) {
    private def this(coreSpec: CoreSpec) = {
      this(
          coreSpec,
          jsHeader = "",
          internalModulePattern = "./" + _.id,
          optimizeBracketSelects = true,
          trackAllGlobalRefs = false,
          minify = false
      )
    }

    private[emitter] val topLevelGlobalRefTracking: GlobalRefTracking =
      if (trackAllGlobalRefs) GlobalRefTracking.All
      else GlobalRefTracking.Dangerous

    def withCoreSpec(coreSpec: CoreSpec): Config =
      copy(coreSpec = coreSpec)

    def withJSHeader(jsHeader: String): Config = {
      require(StandardConfig.isValidJSHeader(jsHeader), jsHeader)
      copy(jsHeader = jsHeader)
    }

    def withInternalModulePattern(internalModulePattern: ModuleID => String): Config =
      copy(internalModulePattern = internalModulePattern)

    def withOptimizeBracketSelects(optimizeBracketSelects: Boolean): Config =
      copy(optimizeBracketSelects = optimizeBracketSelects)

    def withTrackAllGlobalRefs(trackAllGlobalRefs: Boolean): Config =
      copy(trackAllGlobalRefs = trackAllGlobalRefs)

    def withMinify(minify: Boolean): Config =
      copy(minify = minify)

    private def copy(
        coreSpec: CoreSpec = coreSpec,
        jsHeader: String = jsHeader,
        internalModulePattern: ModuleID => String = internalModulePattern,
        optimizeBracketSelects: Boolean = optimizeBracketSelects,
        trackAllGlobalRefs: Boolean = trackAllGlobalRefs,
        minify: Boolean = minify
    ): Config = {
      new Config(coreSpec, jsHeader,
          internalModulePattern, optimizeBracketSelects, trackAllGlobalRefs,
          minify)
    }
  }

  object Config {
    def apply(coreSpec: CoreSpec): Config =
      new Config(coreSpec)
  }

  sealed trait PrePrinter {
    private[Emitter] def prePrint(trees: List[js.Tree], indent: Int): List[js.Tree]
  }

  object PrePrinter {
    object Off extends PrePrinter {
      private[Emitter] def prePrint(trees: List[js.Tree], indent: Int): List[js.Tree] = trees
    }

    object WithoutSourceMap extends PrePrinter {
      private[Emitter] def prePrint(trees: List[js.Tree], indent: Int): List[js.PrintedTree] = {
        if (trees.isEmpty) {
          Nil // Fast path
        } else {
          val jsCodeWriter = new ByteArrayWriter()
          val printer = new Printers.JSTreePrinter(jsCodeWriter, indent)

          trees.foreach(printer.printStat(_))

          js.PrintedTree(jsCodeWriter.toByteArray(), SourceMapWriter.Fragment.Empty) :: Nil
        }
      }
    }

    final class WithSourceMap(fragmentIndex: SourceMapWriter.Index) extends PrePrinter {
      private[Emitter] def prePrint(trees: List[js.Tree], indent: Int): List[js.PrintedTree] = {
        if (trees.isEmpty) {
          Nil // Fast path
        } else {
          val jsCodeWriter = new ByteArrayWriter()
          val smFragmentBuilder = new SourceMapWriter.FragmentBuilder(fragmentIndex)
          val printer = new Printers.JSTreePrinterWithSourceMap(jsCodeWriter, smFragmentBuilder, indent)

          trees.foreach(printer.printStat(_))
          smFragmentBuilder.complete()

          js.PrintedTree(jsCodeWriter.toByteArray(), smFragmentBuilder.result()) :: Nil
        }
      }
    }
  }

  @tailrec
  private def allSame(xs: List[AnyRef], ys: List[AnyRef]): Boolean = {
    xs.isEmpty == ys.isEmpty && {
      xs.isEmpty ||
      ((xs.head eq ys.head) && allSame(xs.tail, ys.tail))
    }
  }

  private final class UncachedDecisions(private val flags: Int) extends AnyVal {
    import UncachedDecisions._

    def hasInstances: Boolean = hasFlag(HasInstances)
    def hasRuntimeTypeInfo: Boolean = hasFlag(HasRuntimeTypeInfo)
    def hasInstanceTests: Boolean = hasFlag(HasInstanceTests)
    def needInstanceTests: Boolean = hasFlag(NeedInstanceTests)
    def needStaticInitialization: Boolean = hasFlag(NeedStaticInitialization)

    @inline private def hasFlag(flag: Int): Boolean =
      (flags & flag) != 0
  }

  private object UncachedDecisions {
    private final val HasInstances = 1 << 0
    private final val HasRuntimeTypeInfo = 1 << 1
    private final val HasInstanceTests = 1 << 2
    private final val NeedInstanceTests = 1 << 3
    private final val NeedStaticInitialization = 1 << 4

    final val Invalid: UncachedDecisions = new UncachedDecisions(-1)

    def apply(
      hasInstances: Boolean,
      hasRuntimeTypeInfo: Boolean,
      hasInstanceTests: Boolean,
      needInstanceTests: Boolean,
      needStaticInitialization: Boolean
    ): UncachedDecisions = {
      def flagIf(cond: Boolean, flag: Int): Int =
        if (cond) flag else 0

      new UncachedDecisions(
        flagIf(hasInstances, HasInstances) |
        flagIf(hasRuntimeTypeInfo, HasRuntimeTypeInfo) |
        flagIf(hasInstanceTests, HasInstanceTests) |
        flagIf(needInstanceTests, NeedInstanceTests) |
        flagIf(needStaticInitialization, NeedStaticInitialization)
      )
    }
  }

  private final class DesugaredClassCache {
    val privateJSFields = new OneTimeCache[WithGlobals[List[js.Tree]]]
    val storeJSSuperClass = new OneTimeCache[WithGlobals[List[js.Tree]]]
    val instanceTests = new OneTimeCache[WithGlobals[List[js.Tree]]]
    val typeData = new InputEqualityCache[Boolean, WithGlobals[List[js.Tree]]]
    val setTypeData = new OneTimeCache[List[js.Tree]]
    val moduleAccessor = new OneTimeCache[WithGlobals[List[js.Tree]]]
    val staticInitialization = new OneTimeCache[List[js.Tree]]
    val staticFields = new OneTimeCache[WithGlobals[List[js.Tree]]]
  }

  private final class GeneratedClass(
      val className: ClassName,
      val main: List[js.Tree],
      val staticFields: List[js.Tree],
      val staticInitialization: List[js.Tree],
      val trackedGlobalRefs: Set[String],
      val changed: Boolean
  )

  private final class OneTimeCache[A >: Null] {
    private[this] var value: A = null
    def getOrElseUpdate(v: => A): A = {
      if (value == null)
        value = v
      value
    }
  }

  /** A cache that depends on an `input: I`, testing with `==`.
   *
   *  @tparam I
   *    the type of input, for which `==` must meaningful
   */
  private final class InputEqualityCache[I, A >: Null] {
    private[this] var lastInput: Option[I] = None
    private[this] var value: A = null

    def getOrElseUpdate(input: I, v: => A): A = {
      if (!lastInput.contains(input)) {
        value = v
        lastInput = Some(input)
      }
      value
    }
  }

  private case class ClassID(
      kind: ClassKind, ancestors: List[ClassName], moduleContext: ModuleContext)

  private def symbolRequirements(config: Config): SymbolRequirement = {
    import config.coreSpec.semantics._
    import CheckedBehavior._

    val factory = SymbolRequirement.factory("emitter")
    import factory._

    def cond(p: Boolean)(v: => SymbolRequirement): SymbolRequirement =
      if (p) v else none()

    def isAnyFatal(behaviors: CheckedBehavior*): Boolean =
      behaviors.contains(Fatal)

    multiple(
        cond(asInstanceOfs != Unchecked) {
          instantiateClass(ClassCastExceptionClass, StringArgConstructorName)
        },

        cond(arrayIndexOutOfBounds != Unchecked) {
          instantiateClass(ArrayIndexOutOfBoundsExceptionClass,
              StringArgConstructorName)
        },

        cond(arrayStores != Unchecked) {
          instantiateClass(ArrayStoreExceptionClass,
              StringArgConstructorName)
        },

        cond(negativeArraySizes != Unchecked) {
          instantiateClass(NegativeArraySizeExceptionClass,
              NoArgConstructorName)
        },

        cond(nullPointers != Unchecked) {
          instantiateClass(NullPointerExceptionClass, NoArgConstructorName)
        },

        cond(stringIndexOutOfBounds != Unchecked) {
          instantiateClass(StringIndexOutOfBoundsExceptionClass,
              IntArgConstructorName)
        },

        cond(isAnyFatal(asInstanceOfs, arrayIndexOutOfBounds, arrayStores,
            negativeArraySizes, nullPointers, stringIndexOutOfBounds)) {
          instantiateClass(UndefinedBehaviorErrorClass,
              ThrowableArgConsructorName)
        },

        cond(moduleInit == Fatal) {
          instantiateClass(UndefinedBehaviorErrorClass,
              StringArgConstructorName)
        },

        // See systemIdentityHashCode in CoreJSLib
        callMethod(BoxedDoubleClass, hashCodeMethodName),
        callMethod(BoxedStringClass, hashCodeMethodName),

        cond(!config.coreSpec.esFeatures.allowBigIntsForLongs) {
          callStaticMethods(LongImpl.RuntimeLongClass, LongImpl.OperatorMethods.toList)
        },

        cond(config.coreSpec.esFeatures.esVersion < ESVersion.ES2015) {
          val cls = FloatingPointBitsPolyfillsClass
          multiple(
            callStaticMethod(cls, floatToBits),
            callStaticMethod(cls, floatFromBits),
            callStaticMethod(cls, doubleToBits),
            callStaticMethod(cls, doubleFromBits)
          )
        }
    )
  }


}
