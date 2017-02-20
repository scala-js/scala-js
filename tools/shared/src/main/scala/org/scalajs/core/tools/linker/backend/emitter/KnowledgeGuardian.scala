/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.backend.emitter

import scala.collection.mutable

import org.scalajs.core.ir.ClassKind
import org.scalajs.core.ir.Trees.{FieldDef, JSNativeLoadSpec}

import org.scalajs.core.tools.linker._

private[emitter] final class KnowledgeGuardian {
  import KnowledgeGuardian._

  private var firstRun: Boolean = true

  private var hasNewRuntimeLong: Boolean = _
  private var isParentDataAccessed: Boolean = _

  private val classes = mutable.Map.empty[String, Class]

  private def askHasNewRuntimeLong(invalidatable: Invalidatable): Boolean =
    hasNewRuntimeLong

  private def askIsParentDataAccessed(invalidatable: Invalidatable): Boolean =
    isParentDataAccessed

  /** Returns `true` if *all* caches should be invalidated.
   *
   *  For global properties that are rarely changed and heavily used (such as
   *  isParentDataAccessed), we do not want to pay the price of the
   *  dependency graph, in terms of memory consumption and time spent
   *  maintaining it. It is a better trade-off to invalidate everything in
   *  the rare events where they do change.
   */
  def update(linkingUnit: LinkingUnit): Boolean = {
    val newIsParentDataAccessed = linkingUnit.globalInfo.isParentDataAccessed
    var newHasNewRuntimeLong: Boolean = false

    // Update classes
    for (linkedClass <- linkingUnit.classDefs) {
      classes.get(linkedClass.encodedName).fold[Unit] {
        // new class
        classes.put(linkedClass.encodedName, new Class(linkedClass))
      } { existingCls =>
        existingCls.update(linkedClass)
      }

      if (linkedClass.encodedName == LongImpl.RuntimeLongClass) {
        newHasNewRuntimeLong = linkedClass.memberMethods.exists { linkedMethod =>
          linkedMethod.tree.name.encodedName == LongImpl.initFromParts
        }
      }
    }

    // Garbage collection
    classes.retain((_, cls) => cls.testAndResetIsAlive())

    val invalidateAll = !firstRun && {
      newIsParentDataAccessed != isParentDataAccessed ||
      newHasNewRuntimeLong != hasNewRuntimeLong
    }
    firstRun = false

    isParentDataAccessed = newIsParentDataAccessed
    hasNewRuntimeLong = newHasNewRuntimeLong

    if (invalidateAll)
      classes.valuesIterator.foreach(_.unregisterAll())
    invalidateAll
  }

  abstract class KnowledgeAccessor extends GlobalKnowledge with Invalidatable {
    /* In theory, a KnowledgeAccessor should *contain* a GlobalKnowledge, not
     * *be* a GlobalKnowledge. We organize it that way to reduce memory
     * footprint and pointer indirections.
     */

    def hasNewRuntimeLong: Boolean =
      askHasNewRuntimeLong(this)

    def isParentDataAccessed: Boolean =
      askIsParentDataAccessed(this)

    def isInterface(className: String): Boolean =
      classes(className).askIsInterface(this)

    def getJSNativeLoadSpec(className: String): Option[JSNativeLoadSpec] =
      classes(className).askJSNativeLoadSpec(this)

    def getSuperClassOfJSClass(className: String): String =
      classes(className).askJSSuperClass(this)

    def getJSClassFieldDefs(className: String): List[FieldDef] =
      classes(className).askJSClassFieldDefs(this)
  }
}

private[emitter] object KnowledgeGuardian {
  private[KnowledgeGuardian] trait Unregisterable {
    def unregister(invalidatable: Invalidatable): Unit
  }

  trait Invalidatable {
    private val _registeredTo = mutable.Set.empty[Unregisterable]

    private[KnowledgeGuardian] def registeredTo(
        unregisterable: Unregisterable): Unit = {
      _registeredTo += unregisterable
    }

    /** To be overridden to perform subclass-specific invalidation.
     *
     *  All overrides should call the default implementation with `super` so
     *  that this `Invalidatable` is unregistered from the dependency graph.
     */
    def invalidate(): Unit = {
      _registeredTo.foreach(_.unregister(this))
      _registeredTo.clear()
    }
  }

  private class Class(initClass: LinkedClass) extends Unregisterable {
    private var isAlive: Boolean = true

    private var isInterface = computeIsInterface(initClass)
    private var jsNativeLoadSpec = computeJSNativeLoadSpec(initClass)
    private var jsSuperClass = computeJSSuperClass(initClass)
    private var jsClassFieldDefs = computeJSClassFieldDefs(initClass)

    private val isInterfaceAskers = mutable.Set.empty[Invalidatable]
    private val jsNativeLoadSpecAskers = mutable.Set.empty[Invalidatable]
    private val jsSuperClassAskers = mutable.Set.empty[Invalidatable]
    private val jsClassFieldDefsAskers = mutable.Set.empty[Invalidatable]

    def update(linkedClass: LinkedClass): Unit = {
      isAlive = true

      val newIsInterface = computeIsInterface(linkedClass)
      if (newIsInterface != isInterface) {
        isInterface = newIsInterface
        invalidateAskers(isInterfaceAskers)
      }

      val newJSNativeLoadSpec = computeJSNativeLoadSpec(linkedClass)
      if (newJSNativeLoadSpec != jsNativeLoadSpec) {
        jsNativeLoadSpec = newJSNativeLoadSpec
        invalidateAskers(jsNativeLoadSpecAskers)
      }

      val newJSSuperClass = computeJSSuperClass(linkedClass)
      if (newJSSuperClass != jsSuperClass) {
        jsSuperClass = newJSSuperClass
        invalidateAskers(jsSuperClassAskers)
      }

      val newJSClassFieldDefs = computeJSClassFieldDefs(linkedClass)
      if (newJSClassFieldDefs != jsClassFieldDefs) {
        jsClassFieldDefs = newJSClassFieldDefs
        invalidateAskers(jsClassFieldDefsAskers)
      }
    }

    private def computeIsInterface(linkedClass: LinkedClass): Boolean =
      linkedClass.kind == ClassKind.Interface

    private def computeJSNativeLoadSpec(linkedClass: LinkedClass): Option[JSNativeLoadSpec] =
      linkedClass.jsNativeLoadSpec

    private def computeJSSuperClass(linkedClass: LinkedClass): String = {
      linkedClass.kind match {
        case ClassKind.JSClass | ClassKind.JSModuleClass =>
          linkedClass.superClass.get.name
        case _ =>
          null
      }
    }

    private def computeJSClassFieldDefs(
        linkedClass: LinkedClass): List[FieldDef] = {
      if (linkedClass.kind == ClassKind.JSClass)
        linkedClass.fields
      else
        Nil
    }

    private def invalidateAskers(askers: mutable.Set[Invalidatable]): Unit = {
      /* Calling `invalidateAndUnregisterFromAll()` will cause the
       * `Invalidatable` to call `unregister()` in this class, which will
       * mutate the `askers` set. Therefore, we cannot directly iterate over
       * `askers`, and need to take a snapshot instead.
       */
      val snapshot = askers.toSeq
      askers.clear()
      snapshot.foreach(_.invalidate())
    }

    def testAndResetIsAlive(): Boolean = {
      val result = isAlive
      isAlive = false
      result
    }

    def askIsInterface(invalidatable: Invalidatable): Boolean = {
      invalidatable.registeredTo(this)
      isInterfaceAskers += invalidatable
      isInterface
    }

    def askJSNativeLoadSpec(invalidatable: Invalidatable): Option[JSNativeLoadSpec] = {
      invalidatable.registeredTo(this)
      jsNativeLoadSpecAskers += invalidatable
      jsNativeLoadSpec
    }

    def askJSSuperClass(invalidatable: Invalidatable): String = {
      invalidatable.registeredTo(this)
      jsSuperClassAskers += invalidatable
      jsSuperClass
    }

    def askJSClassFieldDefs(invalidatable: Invalidatable): List[FieldDef] = {
      invalidatable.registeredTo(this)
      jsClassFieldDefsAskers += invalidatable
      jsClassFieldDefs
    }

    def unregister(invalidatable: Invalidatable): Unit = {
      isInterfaceAskers -= invalidatable
      jsNativeLoadSpecAskers -= invalidatable
      jsSuperClassAskers -= invalidatable
      jsClassFieldDefsAskers -= invalidatable
    }

    /** Call this when we invalidate all caches. */
    def unregisterAll(): Unit = {
      isInterfaceAskers.clear()
      jsNativeLoadSpecAskers.clear()
      jsSuperClassAskers.clear()
      jsClassFieldDefsAskers.clear()
    }
  }
}
