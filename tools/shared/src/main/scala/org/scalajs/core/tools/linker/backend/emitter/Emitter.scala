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

import org.scalajs.core.tools.sem._
import org.scalajs.core.tools.logging._

import org.scalajs.core.tools.javascript.{Trees => js, _}

import org.scalajs.core.tools.linker._
import org.scalajs.core.tools.linker.analyzer.SymbolRequirement
import org.scalajs.core.tools.linker.backend.OutputMode

/** Emits a desugared JS tree to a builder */
final class Emitter private (semantics: Semantics, outputMode: OutputMode,
    internalOptions: InternalOptions) {

  import Emitter._

  def this(semantics: Semantics, outputMode: OutputMode) =
    this(semantics, outputMode, InternalOptions())

  private var classEmitter: ScalaJSClassEmitter = _
  private val classCaches = mutable.Map.empty[List[String], ClassCache]

  private[this] var statsClassesReused: Int = 0
  private[this] var statsClassesInvalidated: Int = 0
  private[this] var statsMethodsReused: Int = 0
  private[this] var statsMethodsInvalidated: Int = 0

  val symbolRequirements: SymbolRequirement =
    Emitter.symbolRequirements(semantics, outputMode.esLevel)

  // Private API for the Closure backend (could be opened if necessary)
  private[backend] def withOptimizeBracketSelects(
      optimizeBracketSelects: Boolean): Emitter = {
    new Emitter(semantics, outputMode,
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
    outputMode match {
      case OutputMode.ECMAScript51Global =>
      case OutputMode.ECMAScript51Isolated | OutputMode.ECMAScript6 =>
        builder.addLine("(function(){")
    }

    builder.addLine("'use strict';")
    builder.addFile(CoreJSLibs.lib(semantics, outputMode))
  }

  def emit(unit: LinkingUnit, builder: JSTreeBuilder, logger: Logger): Unit = {
    classEmitter = new ScalaJSClassEmitter(outputMode, internalOptions, unit)
    startRun()
    try {
      val orderedClasses = unit.classDefs.sortWith(compareClasses)
      for (classInfo <- orderedClasses)
        emitLinkedClass(classInfo, builder)
    } finally {
      endRun(logger)
      classEmitter = null
    }
  }

  def emitPostlude(builder: JSFileBuilder, logger: Logger): Unit = {
    outputMode match {
      case OutputMode.ECMAScript51Global =>
      case OutputMode.ECMAScript51Isolated | OutputMode.ECMAScript6 =>
        builder.addLine("}).call(this);")
    }
  }

  def emitCustomFooter(customFooter: String, builder: JSFileBuilder): Unit =
    emitLines(customFooter, builder)

  private def compareClasses(lhs: LinkedClass, rhs: LinkedClass) = {
    val lhsAC = lhs.ancestors.size
    val rhsAC = rhs.ancestors.size
    if (lhsAC != rhsAC) lhsAC < rhsAC
    else lhs.encodedName.compareTo(rhs.encodedName) < 0
  }

  private def startRun(): Unit = {
    statsClassesReused = 0
    statsClassesInvalidated = 0
    statsMethodsReused = 0
    statsMethodsInvalidated = 0
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
        classEmitter.genMethod(className, m.tree)))
    }

    if (linkedClass.hasInstances && kind.isAnyScalaJSDefinedClass) {
      val ctor = classTreeCache.constructor.getOrElseUpdate(
          classEmitter.genConstructor(linkedClass))

      // Normal methods
      val memberMethods = for (m <- linkedClass.memberMethods) yield {
        val methodCache = classCache.getMethodCache(m.info.encodedName)

        methodCache.getOrElseUpdate(m.version,
            classEmitter.genMethod(className, m.tree))
      }

      // Exported Members
      val exportedMembers = classTreeCache.exportedMembers.getOrElseUpdate(
          classEmitter.genExportedMembers(linkedClass))

      outputMode match {
        case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
          addTree(ctor)
          memberMethods.foreach(addTree)
          addTree(exportedMembers)

        case OutputMode.ECMAScript6 =>
          val allMembersBlock = js.Block(
              ctor :: memberMethods ::: exportedMembers :: Nil)(Position.NoPosition)
          val allMembers = allMembersBlock match {
            case js.Block(members) => members
            case js.Skip()         => Nil
            case oneMember         => List(oneMember)
          }
          addTree(classEmitter.genES6Class(linkedClass, allMembers))
      }
    } else if (kind == ClassKind.Interface) {
      // Default methods
      for (m <- linkedClass.memberMethods) yield {
        val methodCache = classCache.getMethodCache(m.info.encodedName)
        addTree(methodCache.getOrElseUpdate(m.version,
            classEmitter.genDefaultMethod(className, m.tree)))
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
          classEmitter.genTypeData(linkedClass)))
    }

    if (linkedClass.hasInstances && kind.isClass && linkedClass.hasRuntimeTypeInfo)
      addTree(classTreeCache.setTypeData.getOrElseUpdate(
          classEmitter.genSetTypeData(linkedClass)))

    if (linkedClass.kind.hasModuleAccessor)
      addTree(classTreeCache.moduleAccessor.getOrElseUpdate(
          classEmitter.genModuleAccessor(linkedClass)))

    addTree(classTreeCache.classExports.getOrElseUpdate(
        classEmitter.genClassExports(linkedClass)))
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

  // Caching

  private final class ClassCache {
    private[this] var _cache: DesugaredClassCache = null
    private[this] var _lastVersion: Option[String] = None
    private[this] var _cacheUsed = false

    private[this] val _staticCaches = mutable.Map.empty[String, MethodCache]
    private[this] val _methodCaches = mutable.Map.empty[String, MethodCache]

    def startRun(): Unit = {
      _cacheUsed = false
      _staticCaches.valuesIterator.foreach(_.startRun())
      _methodCaches.valuesIterator.foreach(_.startRun())
    }

    def getCache(version: Option[String]): DesugaredClassCache = {
      if (_cache == null || _lastVersion.isEmpty || _lastVersion != version) {
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
        _cache = null

      _staticCaches.nonEmpty || _methodCaches.nonEmpty || _cacheUsed
    }
  }

  private final class MethodCache {
    private[this] var _tree: js.Tree = null
    private[this] var _lastVersion: Option[String] = None
    private[this] var _cacheUsed = false

    def startRun(): Unit = _cacheUsed = false

    def getOrElseUpdate(version: Option[String], v: => js.Tree): js.Tree = {
      if (_tree == null || _lastVersion.isEmpty || _lastVersion != version) {
        statsMethodsInvalidated += 1
        _tree = v
        _lastVersion = version
      } else {
        statsMethodsReused += 1
      }
      _cacheUsed = true
      _tree
    }

    def cleanAfterRun(): Boolean = _cacheUsed
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

        cond(asInstanceOfs == Fatal) {
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
