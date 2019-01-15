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

import org.scalajs.ir.{ClassKind, Position}
import org.scalajs.ir.Trees.JSNativeLoadSpec
import org.scalajs.ir.Definitions.{ObjectClass, decodeClassName, isConstructorName}

import org.scalajs.io._
import org.scalajs.logging._

import org.scalajs.linker._
import org.scalajs.linker.standard._
import org.scalajs.linker.backend.javascript.{Trees => js, _}

import GlobalRefUtils._

/** Emits a desugared JS tree to a builder */
final class Emitter private (config: CommonPhaseConfig,
    internalOptions: InternalOptions) {

  import Emitter._
  import config.coreSpec._

  def this(config: CommonPhaseConfig) = {
    this(config, InternalOptions())
  }

  private val knowledgeGuardian = new KnowledgeGuardian(config)

  private class State(val lastMentionedDangerousGlobalRefs: Set[String]) {
    val jsGen: JSGen = {
      new JSGen(semantics, esFeatures, moduleKind, internalOptions,
          lastMentionedDangerousGlobalRefs)
    }

    val classEmitter: ClassEmitter = new ClassEmitter(jsGen)

    val coreJSLib: WithGlobals[js.Tree] = CoreJSLib.build(jsGen)
  }

  private var state: State = new State(Set.empty)

  private def jsGen: JSGen = state.jsGen
  private def classEmitter: ClassEmitter = state.classEmitter
  private def coreJSLib: WithGlobals[js.Tree] = state.coreJSLib

  private val classCaches = mutable.Map.empty[List[String], ClassCache]

  private[this] var statsClassesReused: Int = 0
  private[this] var statsClassesInvalidated: Int = 0
  private[this] var statsMethodsReused: Int = 0
  private[this] var statsMethodsInvalidated: Int = 0

  val symbolRequirements: SymbolRequirement =
    Emitter.symbolRequirements(config.coreSpec)

  private val needsIIFEWrapper = {
    moduleKind match {
      case ModuleKind.NoModule                             => true
      case ModuleKind.ESModule | ModuleKind.CommonJSModule => false
    }
  }

  // Private API for the Closure backend (could be opened if necessary)
  private[backend] def withOptimizeBracketSelects(
      optimizeBracketSelects: Boolean): Emitter = {
    new Emitter(config,
        internalOptions.withOptimizeBracketSelects(optimizeBracketSelects))
  }

  // Private API for the Closure backend (could be opened if necessary)
  private[backend] def withTrackAllGlobalRefs(
      trackAllGlobalRefs: Boolean): Emitter = {
    new Emitter(config,
        internalOptions.withTrackAllGlobalRefs(trackAllGlobalRefs))
  }

  def emitAll(unit: LinkingUnit, builder: JSLineBuilder,
      logger: Logger): Unit = {
    emitInternal(unit, builder, logger) {
      val topLevelVars = topLevelVarDeclarations(unit)
      if (topLevelVars.nonEmpty) {
        val kw = if (esFeatures.useECMAScript2015) "let " else "var "
        builder.addLine(topLevelVars.mkString(kw, ", ", ";"))
      }

      if (needsIIFEWrapper)
        builder.addLine("(function(){")

      builder.addLine("'use strict';")
    } {
      if (needsIIFEWrapper)
        builder.addLine("}).call(this);")
    }
  }

  /** Emits everything but the core JS lib to the builder, and returns the
   *  top-level var declarations.
   *
   *  This is special for the Closure back-end.
   *
   *  @return
   *    A pair whose first element is the list of top-level variables to be
   *    declared (only non-empty with `NoModule`), and whose second element is
   *    the set of tracked global variables that are accessed.
   */
  private[backend] def emitForClosure(unit: LinkingUnit, builder: JSBuilder,
      logger: Logger): (List[String], Set[String]) = {
    val globalRefs = emitInternal(unit, builder, logger) {
      // no prelude
    } {
      /* When emitting for GCC, we must make sure that every referenced
       * variable is statically declared. This is usually the case, except for
       * some methods of hijacked classes that are accessed by the dispatch
       * functions $dp_xyz. If the target methods are not reachable, they will
       * not be declared, and GCC won't be happy.
       *
       * The following code makes sure to provide declarations for those
       * methods if they are not reachable, to appease GCC.
       *
       * It would be valid and appropriate to introduce those declarations for
       * the non-GCC output as well. However, that is not necessary, so we
       * avoid it simply so that we do not perform useless work.
       *
       * None of this would be necessary if we generated the $dp_xyz functions
       * programmatically, based on the set of reachable methods of hijacked
       * classes, which eventually we should do. But in the meantime, this
       * makes things work.
       */

      import org.scalajs.ir.Definitions._
      import org.scalajs.ir.Position.NoPosition
      import org.scalajs.ir.Trees.MethodDef

      val equals = "equals__O__Z"
      val hashCode = "hashCode__I"
      val compareTo = "compareTo__O__I"

      val requiredDefaultMethodDecls = List(
          BoxedBooleanClass -> List(hashCode, compareTo),
          BoxedCharacterClass -> List(equals, hashCode, compareTo),
          BoxedDoubleClass -> List(
              equals, hashCode, compareTo, "byteValue__B", "shortValue__S",
              "intValue__I", "longValue__J", "floatValue__F", "doubleValue__D"
          ),
          BoxedUnitClass -> List(hashCode),
          BoxedStringClass -> List(
              hashCode, compareTo, "length__I", "charAt__I__C",
              "subSequence__I__I__jl_CharSequence"
          )
      )

      for ((className, requiredMethodNames) <- requiredDefaultMethodDecls) {
        val memberMethods = unit.classDefs
          .find(_.encodedName == className)
          .fold[List[Versioned[MethodDef]]](Nil)(_.memberMethods)
        for {
          methodName <- requiredMethodNames
          if !memberMethods.exists(_.value.name.encodedName == methodName)
        } {
          implicit val pos = NoPosition
          val field = jsGen.envField("f", className + "__" + methodName).ident
          builder.addJSTree(js.VarDef(field, None))
        }
      }
    }

    (topLevelVarDeclarations(unit), globalRefs)
  }

  private def topLevelVarDeclarations(unit: LinkingUnit): List[String] = {
    moduleKind match {
      case ModuleKind.NoModule =>
        val topLevelExportNames = mutable.Set.empty[String]
        for {
          classDef <- unit.classDefs
          export <- classDef.topLevelExports
        } {
          topLevelExportNames += export.value.topLevelExportName
        }
        topLevelExportNames.toList

      case ModuleKind.ESModule | ModuleKind.CommonJSModule =>
        Nil
    }
  }

  /** Returns the set of tracked global refs. */
  private def emitInternal(unit: LinkingUnit, builder: JSBuilder,
      logger: Logger)(
      emitPrelude: => Unit)(
      emitPostlude: => Unit): Set[String] = {
    startRun(unit)
    try {
      val orderedClasses = unit.classDefs.sortWith(compareClasses)
      val WithGlobals(generatedClasses, trackedGlobalRefs) =
        genAllClasses(orderedClasses, logger, secondAttempt = false)

      emitPrelude

      val WithGlobals(coreJSLibTree, coreJSLibTrackedGlobalRefs) = coreJSLib
      builder.addJSTree(coreJSLibTree)

      emitModuleImports(orderedClasses, builder, logger)

      /* Emit all the classes, in the appropriate order:
       *
       * 1. All class definitions, which depend on nothing but their
       *    superclasses.
       * 2. The initialization of $L0, the Long zero, which depends on the
       *    definition of the RuntimeLong class.
       * 3. All static field definitions, which depend on nothing, except those
       *    of type Long which need $L0.
       * 4. All static initializers, which in the worst case can observe some
       *    "zero" state of other static field definitions, but must not
       *    observe a *non-initialized* (undefined) state.
       * 5. All the exports, during which some JS class creation can happen,
       *    causing JS static initializers to run. Those also must not observe
       *    a non-initialized state of other static fields.
       */

      def emitJSTrees(trees: List[js.Tree]): Unit =
        trees.foreach(builder.addJSTree(_))

      for (generatedClass <- generatedClasses)
        emitJSTrees(generatedClass.main)

      if (!jsGen.useBigIntForLongs)
        builder.addJSTree(emitInitializeL0())

      for (generatedClass <- generatedClasses)
        emitJSTrees(generatedClass.staticFields)

      for (generatedClass <- generatedClasses)
        emitJSTrees(generatedClass.staticInitialization)

      for (generatedClass <- generatedClasses)
        emitJSTrees(generatedClass.topLevelExports)

      // Emit the module initializers

      for (moduleInitializer <- unit.moduleInitializers)
        emitModuleInitializer(moduleInitializer, builder)

      emitPostlude

      trackedGlobalRefs ++ coreJSLibTrackedGlobalRefs
    } finally {
      endRun(logger)
    }
  }

  private def emitModuleImports(orderedClasses: List[LinkedClass],
      builder: JSBuilder, logger: Logger): Unit = {

    def foreachImportedModule(f: (String, Position) => Unit): Unit = {
      val encounteredModuleNames = mutable.Set.empty[String]
      for (classDef <- orderedClasses) {
        def addModuleRef(module: String): Unit = {
          if (encounteredModuleNames.add(module))
            f(module, classDef.pos)
        }
        classDef.jsNativeLoadSpec match {
          case None =>
          case Some(JSNativeLoadSpec.Global(_, _)) =>
          case Some(JSNativeLoadSpec.Import(module, _)) =>
            addModuleRef(module)
          case Some(JSNativeLoadSpec.ImportWithGlobalFallback(
              JSNativeLoadSpec.Import(module, _), _)) =>
            addModuleRef(module)
        }
      }
    }

    moduleKind match {
      case ModuleKind.NoModule =>
        var importsFound: Boolean = false

        for (classDef <- orderedClasses) {
          classDef.jsNativeLoadSpec match {
            case Some(JSNativeLoadSpec.Import(module, _)) =>
              val displayName = decodeClassName(classDef.encodedName)
              logger.error(s"$displayName needs to be imported from module " +
                  s"'$module' but module support is disabled.")
              importsFound = true

            case _ =>
              // ok
          }
        }

        if (importsFound) {
          throw new LinkingException(
              "There were module imports without fallback to global " +
              "variables, but module support is disabled.\n" +
              "To enable module support, set `scalaJSLinkerConfig ~= " +
              "(_.withModuleKind(ModuleKind.CommonJSModule))`.")
        }

      case ModuleKind.ESModule =>
        foreachImportedModule { (module, pos0) =>
          implicit val pos = pos0
          val from = js.StringLiteral(module)
          val moduleBinding = jsGen.envModuleField(module).ident
          val importStat = js.ImportNamespace(moduleBinding, from)
          builder.addJSTree(importStat)
        }

      case ModuleKind.CommonJSModule =>
        foreachImportedModule { (module, pos0) =>
          implicit val pos = pos0
          val rhs = js.Apply(js.VarRef(js.Ident("require")),
              List(js.StringLiteral(module)))
          val lhs = jsGen.envModuleField(module)
          val decl = jsGen.genLet(lhs.ident, mutable = false, rhs)
          builder.addJSTree(decl)
        }
    }
  }

  /** Emits the initialization of the global variable `$L0`, which holds the
   *  zero of type `Long`.
   */
  private def emitInitializeL0(): js.Tree = {
    implicit val pos = Position.NoPosition

    // $L0 = new RuntimeLong(0, 0)
    js.Assign(
        jsGen.envField("L0"),
        js.New(jsGen.encodeClassVar(LongImpl.RuntimeLongClass),
            List(js.IntLiteral(0), js.IntLiteral(0)))
    )
  }

  private def compareClasses(lhs: LinkedClass, rhs: LinkedClass) = {
    val lhsAC = lhs.ancestors.size
    val rhsAC = rhs.ancestors.size
    if (lhsAC != rhsAC) lhsAC < rhsAC
    else lhs.encodedName.compareTo(rhs.encodedName) < 0
  }

  private def startRun(unit: LinkingUnit): Unit = {
    statsClassesReused = 0
    statsClassesInvalidated = 0
    statsMethodsReused = 0
    statsMethodsInvalidated = 0

    val invalidateAll = knowledgeGuardian.update(unit)
    if (invalidateAll)
      classCaches.clear()

    classCaches.valuesIterator.foreach(_.startRun())
  }

  private def endRun(logger: Logger): Unit = {
    logger.debug(
        s"Emitter: Class tree cache stats: reused: $statsClassesReused -- "+
        s"invalidated: $statsClassesInvalidated")
    logger.debug(
        s"Emitter: Method tree cache stats: resued: $statsMethodsReused -- "+
        s"invalidated: $statsMethodsInvalidated")
    classCaches.retain((_, c) => c.cleanAfterRun())
  }

  /** Generates all the desugared classes.
   *
   *  If, at the end of the process, the set of accessed dangerous globals has
   *  changed, invalidate *everything* and start over. If at first you don't
   *  succeed, ...
   */
  @tailrec
  private def genAllClasses(orderedClasses: List[LinkedClass], logger: Logger,
      secondAttempt: Boolean): WithGlobals[List[GeneratedClass]] = {

    val objectClass = orderedClasses.find(_.name.name == ObjectClass).get
    val generatedClasses = orderedClasses.map(genClass(_, objectClass))
    val trackedGlobalRefs = generatedClasses.foldLeft(Set.empty[String]) {
      (prev, generatedClass) =>
        unionPreserveEmpty(prev, generatedClass.trackedGlobalRefs)
    }

    val mentionedDangerousGlobalRefs =
      if (!internalOptions.trackAllGlobalRefs) trackedGlobalRefs
      else GlobalRefUtils.keepOnlyDangerousGlobalRefs(trackedGlobalRefs)

    if (mentionedDangerousGlobalRefs == state.lastMentionedDangerousGlobalRefs) {
      WithGlobals(generatedClasses, trackedGlobalRefs)
    } else {
      assert(!secondAttempt,
          "Uh oh! The second attempt gave a different set of dangerous " +
          "global refs than the first one.")

      logger.debug(
          "Emitter: The set of dangerous global refs has changed. " +
          "Going to re-generate the world.")

      state = new State(mentionedDangerousGlobalRefs)
      classCaches.clear()
      genAllClasses(orderedClasses, logger, secondAttempt = true)
    }
  }

  private def genClass(linkedClass: LinkedClass,
      objectClass: LinkedClass): GeneratedClass = {
    val className = linkedClass.encodedName
    val classCache = getClassCache(linkedClass.ancestors)
    val classTreeCache = classCache.getCache(linkedClass.version)
    val kind = linkedClass.kind

    // Global ref management

    var trackedGlobalRefs: Set[String] = Set.empty

    def addGlobalRefs(globalRefs: Set[String]): Unit =
      trackedGlobalRefs = unionPreserveEmpty(globalRefs, trackedGlobalRefs)

    // Main part

    var main: List[js.Tree] = Nil

    def addToMainBase(tree: js.Tree): Unit = main ::= tree

    def addToMain(treeWithGlobals: WithGlobals[js.Tree]): Unit = {
      addToMainBase(treeWithGlobals.value)
      addGlobalRefs(treeWithGlobals.globalVarNames)
    }

    // Static methods
    for (m <- linkedClass.staticMethods) {
      val methodCache = classCache.getStaticCache(m.value.encodedName)

      addToMain(methodCache.getOrElseUpdate(m.version,
          classEmitter.genMethod(className, m.value)(methodCache)))
    }

    // Class definition
    if (linkedClass.hasInstances && kind.isAnyNonNativeClass) {
      val (linkedInlineableInit, linkedMemberMethods0) =
        classEmitter.extractInlineableInit(linkedClass)(classCache)

      // JS constructor
      val ctor = {
        /* The constructor depends both on the class version, and the version
         * of the inlineable init, if there is one.
         */
        val ctorCache = classCache.getConstructorCache()
        val ctorVersion = linkedInlineableInit.fold[Option[String]] {
          linkedClass.version.map("1-" + _)
        } { linkedInit =>
          mergeVersions(linkedClass.version, linkedInit.version).map("2-" + _)
        }
        val initToInline = linkedInlineableInit.map(_.value)
        ctorCache.getOrElseUpdate(ctorVersion,
            classEmitter.genConstructor(linkedClass, initToInline)(ctorCache))
      }

      /* Bridges from Throwable to methods of Object, which are necessary
       * because Throwable is rewired to extend JavaScript's Error instead of
       * j.l.Object.
       */
      val linkedMemberMethods = if (ClassEmitter.shouldExtendJSError(linkedClass)) {
        val existingMethods =
          linkedMemberMethods0.map(_.value.name.encodedName).toSet

        val bridges = for {
          m <- objectClass.memberMethods
          encodedName = m.value.name.encodedName
          if !existingMethods.contains(encodedName) && !isConstructorName(encodedName)
        } yield {
          import org.scalajs.ir.Trees._
          import org.scalajs.ir.Types._

          val methodDef = m.value
          implicit val pos = methodDef.pos

          val methodName = methodDef.name.asInstanceOf[Ident]
          val newBody = ApplyStatically(This()(ClassType(className)),
              ClassRef(ObjectClass), methodName, methodDef.args.map(_.ref))(
              methodDef.resultType)
          val newMethodDef = MethodDef(static = false, methodName,
              methodDef.args, methodDef.resultType, Some(newBody))(
              OptimizerHints.empty, None)
          new Versioned(newMethodDef, m.version)
        }

        linkedMemberMethods0 ++ bridges
      } else {
        linkedMemberMethods0
      }

      // Normal methods
      val memberMethods = for (m <- linkedMemberMethods) yield {
        val methodCache = classCache.getMethodCache(m.value.encodedName)

        methodCache.getOrElseUpdate(m.version,
            classEmitter.genMethod(className, m.value)(methodCache))
      }

      // Exported Members
      val exportedMembers = classTreeCache.exportedMembers.getOrElseUpdate(
          classEmitter.genExportedMembers(linkedClass)(classCache))

      addToMain(classEmitter.buildClass(linkedClass, ctor, memberMethods,
          exportedMembers)(classCache))
    } else if (kind == ClassKind.Interface) {
      // Default methods
      for (m <- linkedClass.memberMethods) yield {
        val methodCache = classCache.getMethodCache(m.value.encodedName)
        addToMain(methodCache.getOrElseUpdate(m.version,
            classEmitter.genDefaultMethod(className, m.value)(methodCache)))
      }
    } else if (kind == ClassKind.HijackedClass) {
      // Hijacked methods
      for (m <- linkedClass.memberMethods) yield {
        val methodCache = classCache.getMethodCache(m.value.encodedName)
        addToMain(methodCache.getOrElseUpdate(m.version,
            classEmitter.genHijackedMethod(className, m.value)(methodCache)))
      }
    }

    if (classEmitter.needInstanceTests(linkedClass)) {
      addToMainBase(classTreeCache.instanceTests.getOrElseUpdate(js.Block(
          classEmitter.genInstanceTests(linkedClass),
          classEmitter.genArrayInstanceTests(linkedClass)
      )(linkedClass.pos)))
    }

    if (linkedClass.hasRuntimeTypeInfo) {
      addToMain(classTreeCache.typeData.getOrElseUpdate(
          classEmitter.genTypeData(linkedClass)(classCache)))
    }

    if (linkedClass.hasInstances && kind.isClass && linkedClass.hasRuntimeTypeInfo)
      addToMainBase(classTreeCache.setTypeData.getOrElseUpdate(
          classEmitter.genSetTypeData(linkedClass)))

    if (linkedClass.kind.hasModuleAccessor)
      addToMainBase(classTreeCache.moduleAccessor.getOrElseUpdate(
          classEmitter.genModuleAccessor(linkedClass)))

    // Static fields

    val staticFields = if (linkedClass.kind.isJSType) {
      Nil
    } else {
      val classCache = getClassCache(linkedClass.ancestors)
      val classTreeCache = classCache.getCache(linkedClass.version)

      classTreeCache.staticFields.getOrElseUpdate(
          classEmitter.genCreateStaticFieldsOfScalaClass(linkedClass)(classCache))
    }

    // Static initialization

    val staticInitialization = if (linkedClass.kind.isJSType) {
      Nil
    } else {
      classEmitter.genStaticInitialization(linkedClass)
    }

    // Top-level exports

    val topLevelExports = if (linkedClass.topLevelExports.isEmpty) {
      Nil
    } else {
      val treeWithGlobals = classTreeCache.topLevelExports.getOrElseUpdate(
          classEmitter.genTopLevelExports(linkedClass)(classCache))
      addGlobalRefs(treeWithGlobals.globalVarNames)
      treeWithGlobals.value
    }

    // Build the result

    new GeneratedClass(
        main.reverse,
        staticFields,
        staticInitialization,
        topLevelExports,
        trackedGlobalRefs
    )
  }

  /** Emits an [[EntryPoint]].
   *
   *  This is done at the very end of the emitted module/script.
   */
  private def emitModuleInitializer(moduleInitializer: ModuleInitializer,
      builder: JSBuilder): Unit = {
    builder.addJSTree(classEmitter.genModuleInitializer(moduleInitializer))
  }

  // Helpers

  private def mergeVersions(v1: Option[String],
      v2: Option[String]): Option[String] = {
    v1.flatMap(s1 => v2.map(s2 => s1.length + "-" + s1 + s2))
  }

  private def getClassTreeCache(linkedClass: LinkedClass): DesugaredClassCache =
    getClassCache(linkedClass.ancestors).getCache(linkedClass.version)

  private def getClassCache(ancestors: List[String]) =
    classCaches.getOrElseUpdate(ancestors, new ClassCache)

  // Caching

  private final class ClassCache extends knowledgeGuardian.KnowledgeAccessor {
    private[this] var _cache: DesugaredClassCache = null
    private[this] var _lastVersion: Option[String] = None
    private[this] var _cacheUsed = false

    private[this] val _staticCaches = mutable.Map.empty[String, MethodCache]
    private[this] val _methodCaches = mutable.Map.empty[String, MethodCache]

    private[this] var _constructorCache: Option[MethodCache] = None

    override def invalidate(): Unit = {
      /* Do not invalidate contained methods, as they have their own
       * invalidation logic.
       */
      super.invalidate()
      _cache = null
      _lastVersion = None
    }

    def startRun(): Unit = {
      _cacheUsed = false
      _staticCaches.valuesIterator.foreach(_.startRun())
      _methodCaches.valuesIterator.foreach(_.startRun())
      _constructorCache.foreach(_.startRun())
    }

    def getCache(version: Option[String]): DesugaredClassCache = {
      if (_cache == null || _lastVersion.isEmpty || _lastVersion != version) {
        invalidate()
        statsClassesInvalidated += 1
        _lastVersion = version
        _cache = new DesugaredClassCache
      } else {
        statsClassesReused += 1
      }
      _cacheUsed = true
      _cache
    }

    def getStaticCache(encodedName: String): MethodCache =
      _staticCaches.getOrElseUpdate(encodedName, new MethodCache)

    def getMethodCache(encodedName: String): MethodCache =
      _methodCaches.getOrElseUpdate(encodedName, new MethodCache)

    def getConstructorCache(): MethodCache = {
      _constructorCache.getOrElse {
        val cache = new MethodCache
        _constructorCache = Some(cache)
        cache
      }
    }

    def cleanAfterRun(): Boolean = {
      _staticCaches.retain((_, c) => c.cleanAfterRun())
      _methodCaches.retain((_, c) => c.cleanAfterRun())

      if (_constructorCache.exists(!_.cleanAfterRun()))
        _constructorCache = None

      if (!_cacheUsed)
        invalidate()

      _staticCaches.nonEmpty || _methodCaches.nonEmpty || _cacheUsed
    }
  }

  private final class MethodCache extends knowledgeGuardian.KnowledgeAccessor {
    private[this] var _tree: WithGlobals[js.Tree] = null
    private[this] var _lastVersion: Option[String] = None
    private[this] var _cacheUsed = false

    override def invalidate(): Unit = {
      super.invalidate()
      _tree = null
      _lastVersion = None
    }

    def startRun(): Unit = _cacheUsed = false

    def getOrElseUpdate(version: Option[String],
        v: => WithGlobals[js.Tree]): WithGlobals[js.Tree] = {
      if (_tree == null || _lastVersion.isEmpty || _lastVersion != version) {
        invalidate()
        statsMethodsInvalidated += 1
        _tree = v
        _lastVersion = version
      } else {
        statsMethodsReused += 1
      }
      _cacheUsed = true
      _tree
    }

    def cleanAfterRun(): Boolean = {
      if (!_cacheUsed)
        invalidate()

      _cacheUsed
    }
  }
}

private object Emitter {
  private final class DesugaredClassCache {
    val exportedMembers = new OneTimeCache[WithGlobals[js.Tree]]
    val instanceTests = new OneTimeCache[js.Tree]
    val typeData = new OneTimeCache[WithGlobals[js.Tree]]
    val setTypeData = new OneTimeCache[js.Tree]
    val moduleAccessor = new OneTimeCache[js.Tree]
    val staticFields = new OneTimeCache[List[js.Tree]]
    val topLevelExports = new OneTimeCache[WithGlobals[List[js.Tree]]]
  }

  private final class GeneratedClass(
      val main: List[js.Tree],
      val staticFields: List[js.Tree],
      val staticInitialization: List[js.Tree],
      val topLevelExports: List[js.Tree],
      val trackedGlobalRefs: Set[String]
  )

  private final class OneTimeCache[A >: Null] {
    private[this] var value: A = null
    def getOrElseUpdate(v: => A): A = {
      if (value == null)
        value = v
      value
    }
  }

  private def symbolRequirements(coreSpec: CoreSpec): SymbolRequirement = {
    import coreSpec.semantics._
    import CheckedBehavior._

    val factory = SymbolRequirement.factory("emitter")
    import factory._

    def cond(p: Boolean)(v: => SymbolRequirement): SymbolRequirement =
      if (p) v else none()

    def assumingES6: Boolean = coreSpec.esFeatures.useECMAScript2015

    multiple(
        instantiateClass("O", "init___"),
        classData("O"),

        instantiateClass("jl_CloneNotSupportedException", "init___"),

        cond(asInstanceOfs != Unchecked) {
          instantiateClass("jl_ClassCastException", "init___T")
        },

        cond(arrayIndexOutOfBounds != Unchecked) {
          instantiateClass("jl_ArrayIndexOutOfBoundsException", "init___T")
        },

        cond(asInstanceOfs == Fatal || arrayIndexOutOfBounds == Fatal) {
          instantiateClass("sjsr_UndefinedBehaviorError", "init___jl_Throwable")
        },

        cond(moduleInit == Fatal) {
          instantiateClass("sjsr_UndefinedBehaviorError", "init___T")
        },

        instantiateClass("jl_Class", "init___O"),

        cond(!coreSpec.esFeatures.allowBigIntsForLongs) {
          multiple(
              instanceTests(LongImpl.RuntimeLongClass),
              instantiateClass(LongImpl.RuntimeLongClass, LongImpl.AllConstructors),
              callMethods(LongImpl.RuntimeLongClass, LongImpl.AllMethods),
              callOnModule(LongImpl.RuntimeLongModuleClass, LongImpl.AllModuleMethods)
          )
        }
    )
  }


}
