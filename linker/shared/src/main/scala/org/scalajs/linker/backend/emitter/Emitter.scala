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
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees.{JSNativeLoadSpec, MemberNamespace}

import org.scalajs.logging._

import org.scalajs.linker.interface._
import org.scalajs.linker.standard._
import org.scalajs.linker.backend.javascript.{Trees => js, _}
import org.scalajs.linker.CollectionsCompat.MutableMapCompatOps

import EmitterNames._
import GlobalRefUtils._

/** Emits a desugared JS tree to a builder */
final class Emitter(config: Emitter.Config) {

  import Emitter._
  import config._

  private val knowledgeGuardian = new KnowledgeGuardian(config)

  private val uncachedKnowledge = new knowledgeGuardian.KnowledgeAccessor {}

  private val nameGen: NameGen = new NameGen

  private class State(val lastMentionedDangerousGlobalRefs: Set[String]) {
    val sjsGen: SJSGen = {
      val jsGen = new JSGen(config)
      val varGen = new VarGen(nameGen, lastMentionedDangerousGlobalRefs)
      new SJSGen(jsGen, nameGen, varGen)
    }

    val classEmitter: ClassEmitter = new ClassEmitter(sjsGen)

    val coreJSLibCache: CoreJSLibCache = new CoreJSLibCache

    val classCaches: mutable.Map[List[ClassName], ClassCache] = mutable.Map.empty
  }

  private var state: State = new State(Set.empty)

  private def jsGen: JSGen = state.sjsGen.jsGen
  private def sjsGen: SJSGen = state.sjsGen
  private def classEmitter: ClassEmitter = state.classEmitter
  private def classCaches: mutable.Map[List[ClassName], ClassCache] = state.classCaches

  private[this] var statsClassesReused: Int = 0
  private[this] var statsClassesInvalidated: Int = 0
  private[this] var statsMethodsReused: Int = 0
  private[this] var statsMethodsInvalidated: Int = 0

  val symbolRequirements: SymbolRequirement =
    Emitter.symbolRequirements(config)

  val injectedIRFiles: Seq[IRFile] = PrivateLibHolder.files

  def emit(unit: LinkingUnit, logger: Logger): Result = {
    val WithGlobals(body, globalRefs) = emitInternal(unit, logger)

    moduleKind match {
      case ModuleKind.NoModule =>
        val topLevelVars = unit.classDefs
          .flatMap(_.topLevelExports)
          .map(_.topLevelExportName)

        val header = {
          val maybeTopLevelVarDecls = if (topLevelVars.nonEmpty) {
            val kw = if (esFeatures.useECMAScript2015) "let " else "var "
            topLevelVars.mkString(kw, ",", ";\n")
          } else {
            ""
          }
          maybeTopLevelVarDecls + "(function(){\n"
        }

        val footer = "}).call(this);\n"

        new Result(header, body, footer, topLevelVars, globalRefs)

      case ModuleKind.ESModule | ModuleKind.CommonJSModule =>
        new Result("", body, "", Nil, globalRefs)
    }
  }

  private def emitInternal(unit: LinkingUnit,
      logger: Logger): WithGlobals[List[js.Tree]] = {
    // Reset caching stats.
    statsClassesReused = 0
    statsClassesInvalidated = 0
    statsMethodsReused = 0
    statsMethodsInvalidated = 0

    // Update GlobalKnowledge.
    val invalidateAll = knowledgeGuardian.update(unit)
    if (invalidateAll) {
      state.coreJSLibCache.invalidate()
      classCaches.clear()
    }

    // Inform caches about new run.
    classCaches.valuesIterator.foreach(_.startRun())

    try {
      emitAvoidGlobalClash(unit, logger, secondAttempt = false)
    } finally {
      // Report caching stats.
      logger.debug(
          s"Emitter: Class tree cache stats: reused: $statsClassesReused -- "+
          s"invalidated: $statsClassesInvalidated")
      logger.debug(
          s"Emitter: Method tree cache stats: reused: $statsMethodsReused -- "+
          s"invalidated: $statsMethodsInvalidated")

      // Inform caches about run completion.
      classCaches.filterInPlace((_, c) => c.cleanAfterRun())
    }
  }

  /** Emits all JavaScript code avoiding clashes with global refs.
   *
   *  If, at the end of the process, the set of accessed dangerous globals has
   *  changed, invalidate *everything* and start over. If at first you don't
   *  succeed, ...
   */
  @tailrec
  private def emitAvoidGlobalClash(unit: LinkingUnit,
      logger: Logger, secondAttempt: Boolean): WithGlobals[List[js.Tree]] = {
    val result = emitOnce(unit, logger)

    val mentionedDangerousGlobalRefs =
      if (!trackAllGlobalRefs) result.globalVarNames
      else GlobalRefUtils.keepOnlyDangerousGlobalRefs(result.globalVarNames)

    if (mentionedDangerousGlobalRefs == state.lastMentionedDangerousGlobalRefs) {
      result
    } else {
      assert(!secondAttempt,
          "Uh oh! The second attempt gave a different set of dangerous " +
          "global refs than the first one.")

      logger.debug(
          "Emitter: The set of dangerous global refs has changed. " +
          "Going to re-generate the world.")

      state = new State(mentionedDangerousGlobalRefs)
      emitAvoidGlobalClash(unit, logger, secondAttempt = true)
    }
  }

  private def emitOnce(unit: LinkingUnit,
      logger: Logger): WithGlobals[List[js.Tree]] = {
    val orderedClasses = unit.classDefs.sortWith(compareClasses)
    val generatedClasses = logger.time("Emitter: Generate classes") {
      orderedClasses.map(genClass(_))
    }

    logger.time("Emitter: Write trees") {
      val WithGlobals(coreJSLib, coreJSLibTrackedGlobalRefs) =
        state.coreJSLibCache.lib

      def classIter = generatedClasses.iterator

      // Emit everything in the appropriate order.
      val treesIter: Iterator[js.Tree] = (
          /* The definitions of the CoreJSLib, which depend on nothing.
           * All classes potentially depend on it.
           */
          Iterator.single(coreJSLib.definitions) ++

          /* Module imports, which depend on nothing.
           * All classes potentially depend on them.
           */
          genModuleImports(orderedClasses, logger) ++

          /* All class definitions, which depend on nothing but their
           * superclasses.
           */
          classIter.flatMap(_.main) ++

          /* The initialization of the CoreJSLib, which depends on the
           * definition of classes (n.b. the RuntimeLong class).
           */
          Iterator.single(coreJSLib.initialization) ++

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
          classIter.flatMap(_.topLevelExports) ++

          /* Module initializers, which by spec run at the end. */
          unit.moduleInitializers.iterator.map(classEmitter.genModuleInitializer(_))
      )

      val trackedGlobalRefs = classIter
        .map(_.trackedGlobalRefs)
        .foldLeft(coreJSLibTrackedGlobalRefs)(unionPreserveEmpty(_, _))

      WithGlobals(treesIter.toList, trackedGlobalRefs)
    }
  }

  private def genModuleImports(orderedClasses: List[LinkedClass],
      logger: Logger): List[js.Tree] = {

    def mapImportedModule(f: (String, Position) => js.Tree): List[js.Tree]  = {
      val builder = mutable.ListBuffer.empty[js.Tree]
      val encounteredModuleNames = mutable.Set.empty[String]
      for (classDef <- orderedClasses) {
        def addModuleRef(module: String): Unit = {
          if (encounteredModuleNames.add(module))
            builder += f(module, classDef.pos)
        }

        def addModuleRefFor(jsNativeLoadSpec: JSNativeLoadSpec): Unit = {
          jsNativeLoadSpec match {
            case JSNativeLoadSpec.Global(_, _) =>
            case JSNativeLoadSpec.Import(module, _) =>
              addModuleRef(module)
            case JSNativeLoadSpec.ImportWithGlobalFallback(
                JSNativeLoadSpec.Import(module, _), _) =>
              addModuleRef(module)
          }
        }

        if (classDef.hasInstances)
          classDef.jsNativeLoadSpec.foreach(addModuleRefFor(_))

        if (classDef.jsNativeMembers.nonEmpty) // likely false
          classDef.jsNativeMembers.foreach(m => addModuleRefFor(m.jsNativeLoadSpec))
      }
      builder.result()
    }

    moduleKind match {
      case ModuleKind.NoModule =>
        var importsFound: Boolean = false

        for (classDef <- orderedClasses) {
          def displayName = classDef.className.nameString

          if (classDef.hasInstances) {
            classDef.jsNativeLoadSpec match {
              case Some(JSNativeLoadSpec.Import(module, _)) =>
                logger.error(s"$displayName needs to be imported from module " +
                    s"'$module' but module support is disabled.")
                importsFound = true

              case _ =>
                // ok
            }
          }

          if (classDef.jsNativeMembers.nonEmpty) { // likely false
            for (nativeMember <- classDef.jsNativeMembers) {
              nativeMember.jsNativeLoadSpec match {
                case JSNativeLoadSpec.Import(module, _) =>
                  logger.error(
                      s"$displayName.${nativeMember.name.name.displayName} " +
                      s"needs to be imported from module '$module' but " +
                      "module support is disabled")
                  importsFound = true

                case _ =>
                  // ok
              }
            }
          }
        }

        if (importsFound) {
          throw new LinkingException(
              "There were module imports without fallback to global " +
              "variables, but module support is disabled.\n" +
              "To enable module support, set `scalaJSLinkerConfig ~= " +
              "(_.withModuleKind(ModuleKind.CommonJSModule))`.")
        } else {
          Nil
        }

      case ModuleKind.ESModule =>
        mapImportedModule { (module, pos0) =>
          implicit val pos = pos0
          val from = js.StringLiteral(module)
          val moduleBinding = sjsGen.envModuleFieldIdent(module)
          js.ImportNamespace(moduleBinding, from)
        }

      case ModuleKind.CommonJSModule =>
        mapImportedModule { (module, pos0) =>
          implicit val pos = pos0
          val rhs = js.Apply(js.VarRef(js.Ident("require")),
              List(js.StringLiteral(module)))
          val lhs = sjsGen.envModuleFieldIdent(module)
          jsGen.genLet(lhs, mutable = false, rhs)
        }
    }
  }

  private def compareClasses(lhs: LinkedClass, rhs: LinkedClass) = {
    val lhsAC = lhs.ancestors.size
    val rhsAC = rhs.ancestors.size
    if (lhsAC != rhsAC) lhsAC < rhsAC
    else lhs.className.compareTo(rhs.className) < 0
  }

  private def genClass(linkedClass: LinkedClass): GeneratedClass = {
    val className = linkedClass.className

    val classCache =
      classCaches.getOrElseUpdate(linkedClass.ancestors, new ClassCache)
    val classTreeCache =
      classCache.getCache(linkedClass.version)

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

    val (linkedInlineableInit, linkedMethods) =
      classEmitter.extractInlineableInit(linkedClass)(classCache)

    // Symbols for private JS fields
    if (kind.isJSClass) {
      val fieldDefs = classTreeCache.privateJSFields.getOrElseUpdate {
        classEmitter.genCreatePrivateJSFieldDefsOfJSClass(linkedClass)
      }
      fieldDefs.foreach(addToMainBase(_))
    }

    // Static-like methods
    for (m <- linkedMethods) {
      val methodDef = m.value
      val namespace = methodDef.flags.namespace

      if (namespace != MemberNamespace.Public) {
        val methodCache =
          classCache.getMethodCache(namespace, methodDef.methodName)

        addToMain(methodCache.getOrElseUpdate(m.version,
            classEmitter.genMethod(className, m.value)(methodCache)))
      }
    }

    // Class definition
    if (linkedClass.hasInstances && kind.isAnyNonNativeClass) {
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
      val linkedMethodsAndBridges = if (ClassEmitter.shouldExtendJSError(linkedClass)) {
        val existingMethods = linkedMethods
          .withFilter(_.value.flags.namespace == MemberNamespace.Public)
          .map(_.value.methodName)
          .toSet

        val bridges = for {
          m <- uncachedKnowledge.methodsInObject()
          methodName = m.value.methodName
          if !existingMethods.contains(methodName)
        } yield {
          import org.scalajs.ir.Trees._
          import org.scalajs.ir.Types._

          val methodDef = m.value
          implicit val pos = methodDef.pos

          val methodName = methodDef.name
          val newBody = ApplyStatically(ApplyFlags.empty,
              This()(ClassType(className)), ObjectClass, methodName,
              methodDef.args.map(_.ref))(
              methodDef.resultType)
          val newMethodDef = MethodDef(MemberFlags.empty, methodName,
              methodDef.originalName, methodDef.args, methodDef.resultType,
              Some(newBody))(
              OptimizerHints.empty, None)
          new Versioned(newMethodDef, m.version)
        }

        linkedMethods ++ bridges
      } else {
        linkedMethods
      }

      // Normal methods
      val memberMethods = for {
        m <- linkedMethodsAndBridges
        if m.value.flags.namespace == MemberNamespace.Public
      } yield {
        val methodCache =
          classCache.getMethodCache(MemberNamespace.Public, m.value.methodName)

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
      for {
        m <- linkedMethods
        if m.value.flags.namespace == MemberNamespace.Public
      } yield {
        val methodCache =
          classCache.getMethodCache(MemberNamespace.Public, m.value.methodName)
        addToMain(methodCache.getOrElseUpdate(m.version,
            classEmitter.genDefaultMethod(className, m.value)(methodCache)))
      }
    } else if (kind == ClassKind.HijackedClass) {
      // Hijacked methods
      for {
        m <- linkedMethods
        if m.value.flags.namespace == MemberNamespace.Public
      } yield {
        val methodCache =
          classCache.getMethodCache(MemberNamespace.Public, m.value.methodName)
        addToMain(methodCache.getOrElseUpdate(m.version,
            classEmitter.genHijackedMethod(className, m.value)(methodCache)))
      }
    }

    if (classEmitter.needInstanceTests(linkedClass)) {
      if (!linkedClass.hasInstances && kind.isClass) {
        /* The isInstanceOf implementation will generate
         * `x instanceof $c_TheClass`, but `$c_TheClass` won't be declared at
         * all. Define it as a fake class to avoid `ReferenceError`s.
         */
        addToMainBase(classEmitter.genFakeClass(linkedClass))
      }

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

  // Helpers

  private def mergeVersions(v1: Option[String],
      v2: Option[String]): Option[String] = {
    v1.flatMap(s1 => v2.map(s2 => "" + s1.length + "-" + s1 + s2))
  }

  // Caching

  private final class ClassCache extends knowledgeGuardian.KnowledgeAccessor {
    private[this] var _cache: DesugaredClassCache = null
    private[this] var _lastVersion: Option[String] = None
    private[this] var _cacheUsed = false

    private[this] val _methodCaches =
      Array.fill(MemberNamespace.Count)(mutable.Map.empty[MethodName, MethodCache])

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
      _methodCaches.foreach(_.valuesIterator.foreach(_.startRun()))
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

    def getMethodCache(namespace: MemberNamespace,
        methodName: MethodName): MethodCache = {
      _methodCaches(namespace.ordinal)
        .getOrElseUpdate(methodName, new MethodCache)
    }

    def getConstructorCache(): MethodCache = {
      _constructorCache.getOrElse {
        val cache = new MethodCache
        _constructorCache = Some(cache)
        cache
      }
    }

    def cleanAfterRun(): Boolean = {
      _methodCaches.foreach(_.filterInPlace((_, c) => c.cleanAfterRun()))

      if (_constructorCache.exists(!_.cleanAfterRun()))
        _constructorCache = None

      if (!_cacheUsed)
        invalidate()

      _methodCaches.exists(_.nonEmpty) || _cacheUsed
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

  private class CoreJSLibCache extends knowledgeGuardian.KnowledgeAccessor {
    private[this] var _lib: WithGlobals[CoreJSLib.Lib] = _

    def lib: WithGlobals[CoreJSLib.Lib] = {
      if (_lib == null)
        _lib = CoreJSLib.build(sjsGen, this)
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
      val body: List[js.Tree],
      val footer: String,
      val topLevelVarDecls: List[String],
      val globalRefs: Set[String]
  )

  /** Configuration for the Emitter. */
  final class Config private (
      val semantics: Semantics,
      val moduleKind: ModuleKind,
      val esFeatures: ESFeatures,
      val optimizeBracketSelects: Boolean,
      val trackAllGlobalRefs: Boolean
  ) {
    private def this(
        semantics: Semantics,
        moduleKind: ModuleKind,
        esFeatures: ESFeatures) = {
      this(
          semantics,
          moduleKind,
          esFeatures,
          optimizeBracketSelects = true,
          trackAllGlobalRefs = false)
    }

    def withSemantics(f: Semantics => Semantics): Config =
      copy(semantics = f(semantics))

    def withModuleKind(moduleKind: ModuleKind): Config =
      copy(moduleKind = moduleKind)

    def withESFeatures(f: ESFeatures => ESFeatures): Config =
      copy(esFeatures = f(esFeatures))

    def withOptimizeBracketSelects(optimizeBracketSelects: Boolean): Config =
      copy(optimizeBracketSelects = optimizeBracketSelects)

    def withTrackAllGlobalRefs(trackAllGlobalRefs: Boolean): Config =
      copy(trackAllGlobalRefs = trackAllGlobalRefs)

    private def copy(
        semantics: Semantics = semantics,
        moduleKind: ModuleKind = moduleKind,
        esFeatures: ESFeatures = esFeatures,
        optimizeBracketSelects: Boolean = optimizeBracketSelects,
        trackAllGlobalRefs: Boolean = trackAllGlobalRefs): Config = {
      new Config(semantics, moduleKind, esFeatures, optimizeBracketSelects,
          trackAllGlobalRefs)
    }
  }

  object Config {
    def apply(coreSpec: CoreSpec): Config =
      new Config(coreSpec.semantics, coreSpec.moduleKind, coreSpec.esFeatures)
  }

  private final class DesugaredClassCache {
    val privateJSFields = new OneTimeCache[List[js.Tree]]
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

  private def symbolRequirements(config: Config): SymbolRequirement = {
    import config.semantics._
    import CheckedBehavior._

    val factory = SymbolRequirement.factory("emitter")
    import factory._

    def cond(p: Boolean)(v: => SymbolRequirement): SymbolRequirement =
      if (p) v else none()

    multiple(
        cond(asInstanceOfs != Unchecked) {
          instantiateClass(ClassCastExceptionClass, StringArgConstructorName)
        },

        cond(arrayIndexOutOfBounds != Unchecked) {
          instantiateClass(ArrayIndexOutOfBoundsExceptionClass,
              StringArgConstructorName)
        },

        cond(asInstanceOfs == Fatal || arrayIndexOutOfBounds == Fatal) {
          instantiateClass(UndefinedBehaviorErrorClass,
              ThrowableArgConsructorName)
        },

        cond(moduleInit == Fatal) {
          instantiateClass(UndefinedBehaviorErrorClass,
              StringArgConstructorName)
        },

        cond(!config.esFeatures.allowBigIntsForLongs) {
          multiple(
              instanceTests(LongImpl.RuntimeLongClass),
              instantiateClass(LongImpl.RuntimeLongClass, LongImpl.AllConstructors.toList),
              callMethods(LongImpl.RuntimeLongClass, LongImpl.AllMethods.toList),
              callOnModule(LongImpl.RuntimeLongModuleClass, LongImpl.AllModuleMethods.toList)
          )
        }
    )
  }


}
