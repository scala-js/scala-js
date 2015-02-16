/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.optimizer

import scala.collection.mutable

import org.scalajs.core.tools.sem._
import org.scalajs.core.tools.logging._

import org.scalajs.core.tools.javascript
import javascript.{Trees => js}

import org.scalajs.core.ir.{ClassKind, Definitions}
import org.scalajs.core.ir.{Trees => ir}

/** Emits a desugared JS tree to a builder */
final class Emitter(semantics: Semantics) {
  import Emitter._

  private val classEmitter = new javascript.ScalaJSClassEmitter(semantics)
  private val classCaches = mutable.Map.empty[String, ClassCache]

  private[this] var statsClassesReused: Int = 0
  private[this] var statsClassesInvalidated: Int = 0
  private[this] var statsMethodsReused: Int = 0
  private[this] var statsMethodsInvalidated: Int = 0

  def emit(unit: LinkingUnit, builder: JSTreeBuilder, logger: Logger): Unit = {
    startRun()
    try {
      for (classInfo <- unit.classDefs.sortWith(compareClasses))
        emitLinkedClass(classInfo, builder)
    } finally {
      endRun(logger)
    }
  }

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
    val classCache = getClassCache(className)
    val classTreeCache = classCache.getCache(linkedClass.version)
    val kind = linkedClass.kind

    // Statics
    for (m <- linkedClass.staticMethods) {
      val methodCache = classCache.getStaticCache(m.info.encodedName)

      addTree(methodCache.getOrElseUpdate(m.version,
        classEmitter.genMethod(className, m.tree)))
    }

    if (linkedClass.hasInstances && kind.isClass) {
      addTree(classTreeCache.constructor.getOrElseUpdate(
          classEmitter.genConstructor(linkedClass)))

      // Normal methods
      for (m <- linkedClass.memberMethods) {
        val methodCache = classCache.getMethodCache(m.info.encodedName)

        addTree(methodCache.getOrElseUpdate(m.version,
            classEmitter.genMethod(className, m.tree)))
      }

      // Exported Members
      addTree(classTreeCache.exportedMembers.getOrElseUpdate(
          classEmitter.genExportedMembers(linkedClass)))
    }

    val needInstanceTests = {
      linkedClass.hasInstanceTests || {
        linkedClass.hasRuntimeTypeInfo &&
        ClassesWhoseDataReferToTheirInstanceTests.contains(linkedClass.encodedName)
      }
    }
    if (needInstanceTests) {
      addTree(classTreeCache.instanceTests.getOrElseUpdate(js.Block(
          classEmitter.genInstanceTests(linkedClass),
          classEmitter.genArrayInstanceTests(linkedClass)
      )(linkedClass.pos)))
    }

    if (linkedClass.hasRuntimeTypeInfo) {
      addTree(classTreeCache.typeData.getOrElseUpdate(
          classEmitter.genTypeData(linkedClass)))
    }

    if (linkedClass.hasInstances && kind.isClass)
      addTree(classTreeCache.setTypeData.getOrElseUpdate(
          classEmitter.genSetTypeData(linkedClass)))

    if (linkedClass.kind == ClassKind.ModuleClass)
      addTree(classTreeCache.moduleAccessor.getOrElseUpdate(
          classEmitter.genModuleAccessor(linkedClass)))

    addTree(classTreeCache.classExports.getOrElseUpdate(
        classEmitter.genClassExports(linkedClass)))
  }

  // Helpers

  private def getClassCache(encodedName: String) =
    classCaches.getOrElseUpdate(encodedName, new ClassCache)

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

object Emitter {
  private val ClassesWhoseDataReferToTheirInstanceTests = {
    Definitions.AncestorsOfHijackedClasses +
    Definitions.ObjectClass + Definitions.StringClass
  }

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
}
