/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.backend.emitter

import scala.annotation.tailrec

import scala.collection.mutable

import org.scalajs.core.ir.{ClassKind, Position}
import org.scalajs.core.ir.Trees.JSNativeLoadSpec
import org.scalajs.core.ir.Definitions.decodeClassName

import org.scalajs.core.tools.sem._
import org.scalajs.core.tools.logging._

import org.scalajs.core.tools.javascript.{Trees => js, _}

import org.scalajs.core.tools.linker._
import org.scalajs.core.tools.linker.analyzer.SymbolRequirement
import org.scalajs.core.tools.linker.backend.{OutputMode, ModuleKind}

/** Emits a desugared JS tree to a builder */
final class Emitter private (semantics: Semantics, outputMode: OutputMode,
    moduleKind: ModuleKind, internalOptions: InternalOptions) {

  import Emitter._

  require(
      outputMode != OutputMode.ECMAScript51Global || moduleKind == ModuleKind.NoModule,
      "The ECMAScript51Global output mode is not compatible with modules")

  def this(semantics: Semantics, outputMode: OutputMode,
      moduleKind: ModuleKind) = {
    this(semantics, outputMode, moduleKind, InternalOptions())
  }

  @deprecated("Use the overload with an explicit ModuleKind.", "0.6.13")
  def this(semantics: Semantics, outputMode: OutputMode) =
    this(semantics, outputMode, ModuleKind.NoModule, InternalOptions())

  private implicit def implicitOutputMode: OutputMode = outputMode

  private val knowledgeGuardian = new KnowledgeGuardian

  private val classEmitter =
    new ScalaJSClassEmitter(semantics, outputMode, internalOptions)

  private val classCaches = mutable.Map.empty[List[String], ClassCache]

  private[this] var statsClassesReused: Int = 0
  private[this] var statsClassesInvalidated: Int = 0
  private[this] var statsMethodsReused: Int = 0
  private[this] var statsMethodsInvalidated: Int = 0

  val symbolRequirements: SymbolRequirement =
    Emitter.symbolRequirements(semantics, outputMode.esLevel)

  private val needsIIFEWrapper = {
    moduleKind match {
      case ModuleKind.NoModule =>
        outputMode match {
          case OutputMode.ECMAScript51Global =>
            false
          case OutputMode.ECMAScript51Isolated | OutputMode.ECMAScript6 =>
            true
        }

      case ModuleKind.CommonJSModule =>
        false
    }
  }

  // Private API for the Closure backend (could be opened if necessary)
  private[backend] def withOptimizeBracketSelects(
      optimizeBracketSelects: Boolean): Emitter = {
    new Emitter(semantics, outputMode, moduleKind,
        internalOptions.withOptimizeBracketSelects(optimizeBracketSelects))
  }

  def emitAll(unit: LinkingUnit, builder: JSFileBuilder,
      logger: Logger): Unit = {
    emitPrelude(builder, logger)
    emit(unit, builder, logger)
    emitPostlude(builder, logger)
  }

  def emitCustomHeader(customHeader: String, builder: JSFileBuilder): Unit =
    emitLines(customHeader, builder)

  def emitPrelude(builder: JSFileBuilder, logger: Logger): Unit = {
    if (needsIIFEWrapper)
      builder.addLine("(function(){")

    builder.addLine("'use strict';")
    builder.addFile(CoreJSLibs.lib(semantics, outputMode, moduleKind))
  }

  def emit(unit: LinkingUnit, builder: JSTreeBuilder, logger: Logger): Unit = {
    startRun(unit)
    try {
      val orderedClasses = unit.classDefs.sortWith(compareClasses)

      emitModuleImports(orderedClasses, builder, logger)

      /* Emit all the classes, in the appropriate order:
       *
       * - First, all class definitions, which depend on nothing but their
       *   superclasses.
       * - Second, all static field definitions, which depend on nothing,
       *   except those of type Long which need to instantiate RuntimeLong.
       * - Third, all static initializers, which in the worst case can observe
       *   some "zero" state of other static field definitions, but must not
       *   observe a *non-initialized* (undefined) state.
       * - Finally, all the exports, during which some JS class creation can
       *   happen, causing JS static initializers to run. Those also must not
       *   observe a non-initialized state of other static fields.
       */

      for (linkedClass <- orderedClasses)
        emitLinkedClass(linkedClass, builder)

      for (linkedClass <- orderedClasses)
        emitLinkedClassStaticFields(linkedClass, builder)

      for (linkedClass <- orderedClasses)
        emitLinkedClassStaticInitializer(linkedClass, builder)

      for (linkedClass <- orderedClasses)
        emitLinkedClassClassExports(linkedClass, builder)
    } finally {
      endRun(logger)
    }
  }

  private def emitModuleImports(orderedClasses: List[LinkedClass],
      builder: JSTreeBuilder, logger: Logger): Unit = {
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
          sys.error("There were module imports, but module support is " +
              "disabled.\nTo enable module support, set scalaJSModuleKind := " +
              "ModuleKind.CommonJSModule.")
        }

      case ModuleKind.CommonJSModule =>
        val jsDesugaring = new JSDesugaring(semantics, outputMode, internalOptions)
        val encounteredModuleNames = mutable.Set.empty[String]
        for (classDef <- orderedClasses) {
          classDef.jsNativeLoadSpec match {
            case None =>
            case Some(JSNativeLoadSpec.Global(_)) =>

            case Some(JSNativeLoadSpec.Import(module, _)) =>
              if (encounteredModuleNames.add(module)) {
                implicit val pos = classDef.pos
                val rhs = js.Apply(js.VarRef(js.Ident("require")),
                    List(js.StringLiteral(module)))
                val lhs = jsDesugaring.envModuleField(module)
                val decl = jsDesugaring.genLet(lhs.ident, mutable = false, rhs)
                builder.addJSTree(decl)
              }
          }
        }
    }
  }

  def emitPostlude(builder: JSFileBuilder, logger: Logger): Unit = {
    if (needsIIFEWrapper)
      builder.addLine("}).call(this);")
  }

  def emitCustomFooter(customFooter: String, builder: JSFileBuilder): Unit =
    emitLines(customFooter, builder)

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

  private def emitLinkedClass(
      linkedClass: LinkedClass, builder: JSTreeBuilder): Unit = {

    def addTree(tree: js.Tree): Unit = builder.addJSTree(tree)

    val className = linkedClass.encodedName
    val classCache = getClassCache(linkedClass.ancestors)
    val classTreeCache = classCache.getCache(linkedClass.version)
    val kind = linkedClass.kind

    // Statics
    for (m <- linkedClass.staticMethods) {
      val methodCache = classCache.getStaticCache(m.info.encodedName)

      addTree(methodCache.getOrElseUpdate(m.version,
          classEmitter.genMethod(className, m.tree)(methodCache)))
    }

    if (linkedClass.hasInstances && kind.isAnyScalaJSDefinedClass) {
      val ctor = classTreeCache.constructor.getOrElseUpdate(
          classEmitter.genConstructor(linkedClass)(classCache))

      // Normal methods
      val memberMethods = for (m <- linkedClass.memberMethods) yield {
        val methodCache = classCache.getMethodCache(m.info.encodedName)

        methodCache.getOrElseUpdate(m.version,
            classEmitter.genMethod(className, m.tree)(methodCache))
      }

      // Exported Members
      val exportedMembers = classTreeCache.exportedMembers.getOrElseUpdate(
          classEmitter.genExportedMembers(linkedClass)(classCache))

      addTree(classEmitter.buildClass(linkedClass, ctor, memberMethods,
          exportedMembers)(classCache))
    } else if (kind == ClassKind.Interface) {
      // Default methods
      for (m <- linkedClass.memberMethods) yield {
        val methodCache = classCache.getMethodCache(m.info.encodedName)
        addTree(methodCache.getOrElseUpdate(m.version,
            classEmitter.genDefaultMethod(className, m.tree)(methodCache)))
      }
    }

    if (classEmitter.needInstanceTests(linkedClass)) {
      addTree(classTreeCache.instanceTests.getOrElseUpdate(js.Block(
          classEmitter.genInstanceTests(linkedClass),
          classEmitter.genArrayInstanceTests(linkedClass)
      )(linkedClass.pos)))
    }

    if (linkedClass.hasRuntimeTypeInfo) {
      addTree(classTreeCache.typeData.getOrElseUpdate(
          classEmitter.genTypeData(linkedClass)(classCache)))
    }

    if (linkedClass.hasInstances && kind.isClass && linkedClass.hasRuntimeTypeInfo)
      addTree(classTreeCache.setTypeData.getOrElseUpdate(
          classEmitter.genSetTypeData(linkedClass)))

    if (linkedClass.kind.hasModuleAccessor)
      addTree(classTreeCache.moduleAccessor.getOrElseUpdate(
          classEmitter.genModuleAccessor(linkedClass)))
  }

  /** Emits the static fields of a linked class.
   *
   *  They are initialized with the zero of their type at this point. It is
   *  the job of static initializers to properly initialize them.
   */
  private def emitLinkedClassStaticFields(linkedClass: LinkedClass,
      builder: JSTreeBuilder): Unit = {

    if (!linkedClass.kind.isJSType) {
      val classCache = getClassCache(linkedClass.ancestors)
      val classTreeCache = classCache.getCache(linkedClass.version)

      builder.addJSTree(classTreeCache.staticFields.getOrElseUpdate(
          classEmitter.genCreateStaticFieldsOfScalaClass(linkedClass)(classCache)))
    }
  }

  /** Emits the static initializer of a linked class, if any. */
  private def emitLinkedClassStaticInitializer(linkedClass: LinkedClass,
      builder: JSTreeBuilder): Unit = {

    if (!linkedClass.kind.isJSType)
      builder.addJSTree(classEmitter.genStaticInitialization(linkedClass))
  }

  /** Emits the class exports of a linked class.
   *
   *  This is done after everything else has been emitted for all the classes
   *  in the program. That is necessary because class exports can call class
   *  value accessors, which may have unknown circular references.
   */
  private def emitLinkedClassClassExports(linkedClass: LinkedClass,
      builder: JSTreeBuilder): Unit = {

    /* `if` to avoid looking up the caches for nothing. Probably worth doing
     * because only few classes have class exports.
     */
    if (linkedClass.classExports.nonEmpty) {
      val classCache = getClassCache(linkedClass.ancestors)
      val classTreeCache = classCache.getCache(linkedClass.version)

      builder.addJSTree(classTreeCache.classExports.getOrElseUpdate(
          classEmitter.genClassExports(linkedClass)(classCache)))
    }
  }

  // Helpers

  private def getClassTreeCache(linkedClass: LinkedClass): DesugaredClassCache =
    getClassCache(linkedClass.ancestors).getCache(linkedClass.version)

  private def getClassCache(ancestors: List[String]) =
    classCaches.getOrElseUpdate(ancestors, new ClassCache)

  private def emitLines(str: String, builder: JSFileBuilder): Unit = {
    @tailrec def emitNextLine(index: Int): Unit = {
      val endOfLine = str.indexOf('\n', index)
      if (endOfLine != -1) {
        builder.addLine(str.substring(index, endOfLine))
        emitNextLine(endOfLine + 1)
      } else {
        builder.addLine(str.substring(index, str.length))
      }
    }
    if (str != "")
      emitNextLine(0)
  }

  // Private API for Rhino

  private[scalajs] object rhinoAPI { // scalastyle:ignore
    /** A GlobalKnowledge that never tracks dependencies. This can be used in
     *  cases where we do not use any cache, which is what `genClassDef()` in
     *  this class does.
     */
    private val globalKnowledge: GlobalKnowledge =
      new knowledgeGuardian.KnowledgeAccessor {}

    def initialize(linkingUnit: LinkingUnit): Unit =
      startRun(linkingUnit)

    def getHeaderFile(): org.scalajs.core.tools.io.VirtualJSFile =
      CoreJSLibs.lib(semantics, outputMode, moduleKind)

    def genClassDef(linkedClass: LinkedClass): js.Tree =
      classEmitter.genClassDefForRhino(linkedClass)(globalKnowledge)
  }

  // Caching

  private final class ClassCache extends knowledgeGuardian.KnowledgeAccessor {
    private[this] var _cache: DesugaredClassCache = null
    private[this] var _lastVersion: Option[String] = None
    private[this] var _cacheUsed = false

    private[this] val _staticCaches = mutable.Map.empty[String, MethodCache]
    private[this] val _methodCaches = mutable.Map.empty[String, MethodCache]

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

    def cleanAfterRun(): Boolean = {
      _staticCaches.retain((_, c) => c.cleanAfterRun())
      _methodCaches.retain((_, c) => c.cleanAfterRun())

      if (!_cacheUsed)
        invalidate()

      _staticCaches.nonEmpty || _methodCaches.nonEmpty || _cacheUsed
    }
  }

  private final class MethodCache extends knowledgeGuardian.KnowledgeAccessor {
    private[this] var _tree: js.Tree = null
    private[this] var _lastVersion: Option[String] = None
    private[this] var _cacheUsed = false

    override def invalidate(): Unit = {
      super.invalidate()
      _tree = null
      _lastVersion = None
    }

    def startRun(): Unit = _cacheUsed = false

    def getOrElseUpdate(version: Option[String], v: => js.Tree): js.Tree = {
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

// The only reason this is not private is that Rhino needs it
private[scalajs] object Emitter {
  private final class DesugaredClassCache {
    val constructor = new OneTimeCache[js.Tree]
    val exportedMembers = new OneTimeCache[js.Tree]
    val instanceTests = new OneTimeCache[js.Tree]
    val typeData = new OneTimeCache[js.Tree]
    val setTypeData = new OneTimeCache[js.Tree]
    val moduleAccessor = new OneTimeCache[js.Tree]
    val staticFields = new OneTimeCache[js.Tree]
    val classExports = new OneTimeCache[js.Tree]
  }

  private final class OneTimeCache[A >: Null] {
    private[this] var value: A = null
    def getOrElseUpdate(v: => A): A = {
      if (value == null)
        value = v
      value
    }
  }

  // The only reason this is not private is that Rhino needs it
  private[scalajs] def symbolRequirements(semantics: Semantics,
      esLevel: ESLevel): SymbolRequirement = {
    import semantics._
    import CheckedBehavior._

    val factory = SymbolRequirement.factory("emitter")
    import factory._

    def cond(p: Boolean)(v: => SymbolRequirement): SymbolRequirement =
      if (p) v else none()

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

        instantiateClass("jl_Class", "init___jl_ScalaJSClassData"),

        callOnModule("jl_Double$", "compare__D__D__I"),
        callOnModule("sjsr_RuntimeString$", "hashCode__T__I"),

        instanceTests(LongImpl.RuntimeLongClass),
        instantiateClass(LongImpl.RuntimeLongClass, LongImpl.AllConstructors),
        callMethods(LongImpl.RuntimeLongClass, LongImpl.AllMethods),

        callOnModule(LongImpl.RuntimeLongModuleClass, LongImpl.AllModuleMethods),

        cond(semantics.strictFloats && esLevel == ESLevel.ES5) {
          callOnModule("sjsr_package$", "froundPolyfill__D__D")
        },
        callOnModule("sjsr_Bits$", "numberHashCode__D__I")
    )
  }


}
