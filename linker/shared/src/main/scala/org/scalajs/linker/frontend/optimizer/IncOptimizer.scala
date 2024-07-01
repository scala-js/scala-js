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

package org.scalajs.linker.frontend.optimizer

import scala.annotation.{switch, tailrec}

import scala.collection.mutable

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

import org.scalajs.ir._
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.Position.NoPosition

import org.scalajs.logging._

import org.scalajs.linker._
import org.scalajs.linker.backend.emitter.LongImpl
import org.scalajs.linker.frontend.LinkingUnit
import org.scalajs.linker.interface.{CheckedBehavior, ModuleKind}
import org.scalajs.linker.standard._
import org.scalajs.linker.CollectionsCompat._

import OptimizerCore.InlineableFieldBodies.FieldBody

/** Incremental optimizer.
 *
 *  An incremental optimizer optimizes a [[LinkingUnit]]
 *  in an incremental way.
 *
 *  It maintains state between runs to do a minimal amount of work on every
 *  run, based on detecting what parts of the program must be re-optimized,
 *  and keeping optimized results from previous runs for the rest.
 *
 *  A general note about use of ConcurrentHashMap[T, Unit] as concurrent sets:
 *  It would seem better to use ConcurrentHashMap.newKeySet() which is
 *  specifically designed for this purpose. However, the views alone use up 4 MB
 *  of shallow size on the test suite at the time of writing. Therefore, we give
 *  up on the convenience API and use the underlying ConcurrentHashMap directly.
 *
 *  A similar argument applies to usages of keySet(): It appears that the
 *  implementation holds on to the key set once it is created, resulting in
 *  unnecessary memory usage.
 *
 *  @param semantics Required Scala.js Semantics
 *  @param esLevel ECMAScript level
 */
final class IncOptimizer private[optimizer] (config: CommonPhaseConfig, collOps: AbsCollOps) {

  import IncOptimizer._

  val symbolRequirements: SymbolRequirement = {
    val factory = SymbolRequirement.factory("optimizer")
    import factory._

    callMethods(LongImpl.RuntimeLongClass, LongImpl.AllIntrinsicMethods.toList)
  }

  /** Are we in batch mode? I.e., are we running from scratch?
   *  Various parts of the algorithm can be skipped entirely when running in
   *  batch mode.
   */
  private var batchMode: Boolean = false

  private var objectClass: Class = _
  private val classes = new ConcurrentHashMap[ClassName, Class]
  private val interfaces = new ConcurrentHashMap[ClassName, InterfaceType]
  private val topLevelExports = new JSTopLevelMethodContainer

  private var methodsToProcess = collOps.emptyAddable[Processable]

  @inline
  private def getInterface(className: ClassName): InterfaceType =
    interfaces.get(className)

  @inline
  private def classOrElse[T >: Class](className: ClassName, default: => T): T = {
    val clazz = classes.get(className)
    if (clazz != null) clazz
    else default
  }

  /** Update the incremental analyzer with a new run. */
  def update(unit: LinkingUnit, logger: Logger): List[(ClassDef, Version)] = {
    batchMode = objectClass == null
    logger.debug(s"Optimizer: Batch mode: $batchMode")

    logger.time("Optimizer: Incremental part") {
      /* UPDATE PASS */
      updateAndTagEverything(unit)
    }

    logger.time("Optimizer: Elidable constructors") {
      /** ELIDABLE CTORS PASS */
      updateElidableConstructors()
    }

    logger.time("Optimizer: Optimizer part") {
      /* PROCESS PASS */
      processAllTaggedMethods(logger)
    }

    val groupedTopLevelExports = unit.topLevelExports.groupBy(_.owningClass)

    for {
      linkedClass <- unit.classDefs
    } yield {
      val topLevelExports = groupedTopLevelExports.getOrElse(linkedClass.className, Nil)
      optimizedClass(linkedClass, topLevelExports)
    }
  }

  private def optimizedClass(linkedClass: LinkedClass,
      tles: List[LinkedTopLevelExport]): (ClassDef, Version) = {
    val className = linkedClass.className
    val interface = getInterface(className)

    val publicContainer = classOrElse(className, {
      /* For interfaces, we need to look at default methods.
       * For other kinds of classes, the public namespace is necessarily
       * empty.
       */
      val container = interface.staticLike(MemberNamespace.Public)
      assert(
          linkedClass.kind == ClassKind.Interface || container.methods.isEmpty,
          linkedClass.className -> linkedClass.kind)
      container
    })

    val newMethods = for (m <- linkedClass.methods) yield {
      val namespace = m.flags.namespace
      val container =
        if (namespace == MemberNamespace.Public) publicContainer
        else interface.staticLike(namespace)
      container.methods(m.methodName).optimizedDef
    }

    val newTopLevelExports = tles.map { tle =>
      tle.tree match {
        case method: TopLevelMethodExportDef =>
          topLevelExports.optimizedMethod(method.moduleID, method.topLevelExportName)

        case tree =>
          tree
      }
    }

    val classDef = ClassDef(
      linkedClass.name,
      OriginalName.NoOriginalName,
      linkedClass.kind,
      linkedClass.jsClassCaptures,
      linkedClass.superClass,
      linkedClass.interfaces,
      linkedClass.jsSuperClass,
      linkedClass.jsNativeLoadSpec,
      linkedClass.fields,
      newMethods,
      interface.optimizedJSConstructorDef(),
      interface.optimizedExportedMembers(),
      linkedClass.jsNativeMembers,
      newTopLevelExports
    )(linkedClass.optimizerHints)(linkedClass.pos)

    (classDef, linkedClass.version)
  }

  /** Incremental part: update state and detect what needs to be re-optimized.
   *  UPDATE PASS ONLY. (This IS the update pass).
    */
  private def updateAndTagEverything(unit: LinkingUnit): Unit = {
    updateAndTagClasses(unit.classDefs)
    topLevelExports.updateWith(unit.topLevelExports)
  }

  private def updateAndTagClasses(linkedClasses: List[LinkedClass]): Unit = {
    val neededInterfaces = new ConcurrentHashMap[ClassName, LinkedClass]
    val neededClasses = new ConcurrentHashMap[ClassName, LinkedClass]
    for (linkedClass <- linkedClasses) {
      neededInterfaces.put(linkedClass.className, linkedClass)

      if (linkedClass.hasInstances &&
          (linkedClass.kind.isClass || linkedClass.kind == ClassKind.HijackedClass)) {
        neededClasses.put(linkedClass.className, linkedClass)
      }
    }

    /* Remove deleted interfaces, and update existing ones.
     * We don't even have to notify callers in case of additions or removals
     * because callers have got to be invalidated by themselves.
     * Only changed methods need to trigger notifications.
     *
     * Non-batch mode only.
     */
    assert(!batchMode || interfaces.isEmpty())
    if (!batchMode) {
      interfaces.forEach(collOps.parThreshold, { (className, interface) =>
        val linkedClass = neededInterfaces.remove(className)
        if (linkedClass == null) {
          interface.delete()
          interfaces.remove(className)
        } else {
          interface.updateWith(linkedClass)
        }
      })
    }

    /* Add new interfaces.
     * Easy, we don't have to notify anyone.
     */
    neededInterfaces.forEachValue(collOps.parThreshold, { linkedClass =>
      val interface = new InterfaceType(linkedClass)
      interfaces.put(interface.className, interface)
    })

    if (!batchMode) {
      /* Class removals:
       * * If a class is deleted or moved, delete its entire subtree (because
       *   all its descendants must also be deleted or moved).
       * * If an existing class was instantiated but is no more, notify callers
       *   of its methods.
       *
       * Non-batch mode only.
       */
      val objectClassStillExists =
        objectClass.walkClassesForDeletions(className => Option(neededClasses.get(className)))
      assert(objectClassStillExists, "Uh oh, java.lang.Object was deleted!")

      /* Class changes:
       * * Delete removed methods, update existing ones, add new ones
       * * Update the list of ancestors
       * * Class newly instantiated
       *
       * Non-batch mode only.
       */
      objectClass.walkForChanges(neededClasses.remove(_), Set.empty)
    }

    /* Class additions:
     * * Add new classes (including those that have moved from elsewhere).
     * In batch mode, we avoid doing notifications.
     */

    // Group children by (immediate) parent
    val newChildrenByParent = new ConcurrentHashMap[ClassName, collOps.Addable[LinkedClass]]

    neededClasses.forEachValue(collOps.parThreshold, { linkedClass =>
      linkedClass.superClass.fold[Unit] {
        assert(batchMode, "Trying to add java.lang.Object in incremental mode")
        objectClass = new Class(None, linkedClass)
        classes.put(linkedClass.className, objectClass)
      } { superClassName =>
        val addable = newChildrenByParent
          .computeIfAbsent(superClassName.name, _ => collOps.emptyAddable)

        collOps.add(addable, linkedClass)
      }
    })

    val getNewChildren = { (name: ClassName) =>
      val acc = newChildrenByParent.get(name)
      if (acc == null) collOps.emptyParIterable[LinkedClass]
      else collOps.finishAdd(acc)
    }

    // Walk the tree to add children
    if (batchMode) {
      objectClass.walkForAdditions(getNewChildren)
    } else {
      newChildrenByParent.forEachKey(1, { parentName =>
        val parent = classes.get(parentName)
        if (parent != null)
          parent.walkForAdditions(getNewChildren)
      })
    }

  }

  /** Elidable constructors: compute the fix point of hasElidableConstructors.
   *
   *  ELIDABLE CTORS PASS ONLY. (This IS the elidable ctors pass).
   */
  private def updateElidableConstructors(): Unit = {
    import ElidableConstructorsInfo._

    val toProcessStack = mutable.ArrayBuffer.empty[Class]

    /* Invariants for this algo:
     * - when a class is in the stack, its elidableConstructorsInfo was set
     *   to AcyclicElidable,
     * - when a class has `DependentOn` as its info, its
     *   `elidableConstructorsRemainingDependenciesCount` is the number of
     *   classes in its `dependencies` that have not yet been *processed* as
     *   AcyclicElidable.
     *
     * During this algorithm, the info can transition from DependentOn to
     *
     * - NotElidable, if its getter dependencies were not satisfied.
     * - AcyclicElidable.
     *
     * Other transitions are not possible.
     */

    def isGetter(classAndMethodName: (ClassName, MethodName)): Boolean = {
      val (className, methodName) = classAndMethodName
      classes.get(className).lookupMethod(methodName).exists { m =>
        m.originalDef.body match {
          case Some(Select(This(), _)) => true
          case _                          => false
        }
      }
    }

    /* Initialization:
     * - Prune classes with unsatisfied getter dependencies
     * - Build reverse dependencies
     * - Initialize `elidableConstructorsRemainingDependenciesCount` for `DependentOn` classes
     * - Initialize the stack with dependency-free classes
     */
    classes.forEachValue(Long.MaxValue, { cls =>
      cls.elidableConstructorsInfo match {
        case DependentOn(deps, getterDeps) =>
          if (!getterDeps.forall(isGetter(_))) {
            cls.elidableConstructorsInfo = NotElidable
          } else {
            if (deps.isEmpty) {
              cls.elidableConstructorsInfo = AcyclicElidable
              toProcessStack += cls
            } else {
              cls.elidableConstructorsRemainingDependenciesCount = deps.size
              deps.foreach(dep => classes.get(dep).elidableConstructorsDependents += cls)
            }
          }
        case AcyclicElidable =>
          toProcessStack += cls
        case NotElidable =>
          ()
      }
    })

    /* Propagate AcyclicElidable
     * When a class `cls` is on the stack, it is known to be AcyclicElidable.
     * Go to all its dependents and decrement their count of remaining
     * dependencies. If the count reaches 0, then all the dependencies of the
     * class are known to be AcyclicElidable, and so the new class is known to
     * be AcyclicElidable.
     */
    while (toProcessStack.nonEmpty) {
      val cls = toProcessStack.remove(toProcessStack.size - 1)

      for (dependent <- cls.elidableConstructorsDependents) {
        dependent.elidableConstructorsInfo match {
          case DependentOn(_, _) =>
            dependent.elidableConstructorsRemainingDependenciesCount -= 1
            if (dependent.elidableConstructorsRemainingDependenciesCount == 0) {
              dependent.elidableConstructorsInfo = AcyclicElidable
              toProcessStack += dependent
            }
          case NotElidable =>
            ()
          case AcyclicElidable =>
            throw new AssertionError(
              s"Unexpected dependent link from class ${cls.className.nameString} " +
              s"to ${dependent.className.nameString} which is AcyclicElidable"
            )
        }
      }
    }

    // Set the final value of hasElidableConstructors
    classes.forEachValue(Long.MaxValue, _.setHasElidableConstructors())
  }

  /** Optimizer part: process all methods that need reoptimizing.
   *  PROCESS PASS ONLY. (This IS the process pass).
   */
  private def processAllTaggedMethods(logger: Logger): Unit = {
    val methods = collOps.finishAdd(methodsToProcess)
    methodsToProcess = collOps.emptyAddable

    val count = collOps.count(methods)(!_.deleted)
    logger.debug(s"Optimizer: Optimizing $count methods.")
    collOps.foreach(methods)(_.process())
  }

  /** Base class for [[IncOptimizer.Class]] and
   *  [[IncOptimizer.StaticLikeNamespace]].
   */
  private abstract class MethodContainer(linkedClass: LinkedClass,
      val myInterface: InterfaceType, val namespace: MemberNamespace) {

    val className: ClassName = linkedClass.className

    def untrackedThisType: Type =
      if (namespace.isStatic) NoType
      else myInterface.untrackedInstanceThisType

    val methods = mutable.Map.empty[MethodName, MethodImpl]

    updateWith(linkedClass)

    /** UPDATE PASS ONLY. Global concurrency safe but not on same instance */
    def updateWith(linkedClass: LinkedClass):
        (Set[MethodName], Set[MethodName], Set[MethodName]) = {

      val addedMethods = Set.newBuilder[MethodName]
      val changedMethods = Set.newBuilder[MethodName]
      val deletedMethods = Set.newBuilder[MethodName]

      val applicableNamespaceOrdinal = this match {
        case _: StaticLikeNamespace
            if namespace == MemberNamespace.Public &&
                (linkedClass.kind.isClass || linkedClass.kind == ClassKind.HijackedClass) =>
          /* The public non-static namespace for a class is always empty,
           * because its would-be content must be handled by the `Class`
           * instead.
           */
          -1 // different from all `ordinal` values of legit namespaces
        case _ =>
          namespace.ordinal
      }
      val linkedMethodDefs = linkedClass.methods.withFilter {
        _.flags.namespace.ordinal == applicableNamespaceOrdinal
      }

      val newMethodNames = linkedMethodDefs.map(_.methodName).toSet
      val methodSetChanged = methods.keySet != newMethodNames
      if (methodSetChanged) {
        // Remove deleted methods
        methods.filterInPlace { (methodName, method) =>
          if (newMethodNames.contains(methodName)) {
            true
          } else {
            deletedMethods += methodName
            method.delete()
            false
          }
        }
      }

      for (linkedMethodDef <- linkedMethodDefs) {
        val methodName = linkedMethodDef.methodName

        methods.get(methodName).fold {
          addedMethods += methodName
          val method = new MethodImpl(this, methodName)
          method.updateWith(linkedMethodDef)
          methods(methodName) = method
          method
        } { method =>
          if (method.updateWith(linkedMethodDef))
            changedMethods += methodName
          method
        }
      }

      (addedMethods.result(), changedMethods.result(), deletedMethods.result())
    }

    def lookupMethod(methodName: MethodName): Option[MethodImpl]

    override def toString(): String =
      namespace.prefixString + className.nameString
  }

  /** Class in the class hierarchy (not an interface).
   *  A class may be a module class.
   *  A class knows its superclass and the interfaces it implements. It also
   *  maintains a list of its direct subclasses, so that the instances of
   *  [[Class]] form a tree of the class hierarchy.
   */
  private final class Class(val superClass: Option[Class], linkedClass: LinkedClass)
      extends MethodContainer(linkedClass, getInterface(linkedClass.className), MemberNamespace.Public)
      with Unregisterable {

    if (className == ObjectClass) {
      assert(superClass.isEmpty)
      assert(objectClass == null)
    } else {
      assert(superClass.isDefined)
    }

    /** Parent chain from this to Object. */
    val parentChain: List[Class] =
      this :: superClass.fold[List[Class]](Nil)(_.parentChain)

    /** Reverse parent chain from Object to this. */
    val reverseParentChain: List[Class] =
      parentChain.reverse

    var interfaces: Set[InterfaceType] = linkedClass.ancestors.map(getInterface).toSet
    var subclasses: collOps.ParIterable[Class] = collOps.emptyParIterable
    var isInstantiated: Boolean = linkedClass.hasInstances

    // Temporary information used to eventually derive `hasElidableConstructors`
    var elidableConstructorsInfo: ElidableConstructorsInfo =
      computeElidableConstructorsInfo(linkedClass)
    val elidableConstructorsDependents: mutable.ArrayBuffer[Class] = mutable.ArrayBuffer.empty
    var elidableConstructorsRemainingDependenciesCount: Int = 0

    /** True if *all* constructors of this class are recursively elidable. */
    private var hasElidableConstructors: Boolean =
      elidableConstructorsInfo != ElidableConstructorsInfo.NotElidable // initial educated guess
    private val hasElidableConstructorsAskers = new ConcurrentHashMap[Processable, Unit]

    var fields: List[AnyFieldDef] = linkedClass.fields
    var fieldsRead: Set[FieldName] = linkedClass.fieldsRead
    var tryNewInlineable: Option[OptimizerCore.InlineableClassStructure] = None

    /** The "bodies" of fields that can "inlined", *provided that* the enclosing
     *  module class has elidable constructors.
     */
    private var inlineableFieldBodies: OptimizerCore.InlineableFieldBodies =
      computeInlineableFieldBodies(linkedClass)
    private val inlineableFieldBodiesAskers = new ConcurrentHashMap[Processable, Unit]

    setupAfterCreation(linkedClass)

    override def toString(): String =
      className.nameString

    /** Walk the class hierarchy tree for deletions.
     *  This includes "deleting" classes that were previously instantiated but
     *  are no more.
     *  UPDATE PASS ONLY. Not concurrency safe on same instance.
     */
    def walkClassesForDeletions(
        getLinkedClassIfNeeded: ClassName => Option[LinkedClass]): Boolean = {
      def sameSuperClass(linkedClass: LinkedClass): Boolean =
        superClass.map(_.className) == linkedClass.superClass.map(_.name)

      getLinkedClassIfNeeded(className) match {
        case Some(linkedClass) if sameSuperClass(linkedClass) =>
          // Class still exists. Recurse.
          subclasses = collOps.filter(subclasses)(
              _.walkClassesForDeletions(getLinkedClassIfNeeded))
          if (isInstantiated && !linkedClass.hasInstances)
            notInstantiatedAnymore()
          true
        case _ =>
          // Class does not exist or has been moved. Delete the entire subtree.
          deleteSubtree()
          false
      }
    }

    /** Delete this class and all its subclasses. UPDATE PASS ONLY. */
    def deleteSubtree(): Unit = {
      delete()
      collOps.foreach(subclasses)(_.deleteSubtree())
    }

    /** UPDATE PASS ONLY. */
    private def delete(): Unit = {
      if (isInstantiated)
        notInstantiatedAnymore()
      for (method <- methods.values)
        method.delete()
      classes.remove(className)
      /* Note: no need to tag methods that call *statically* one of the methods
       * of the deleted classes, since they've got to be invalidated by
       * themselves.
       */
    }

    /** UPDATE PASS ONLY. */
    def notInstantiatedAnymore(): Unit = {
      assert(isInstantiated)
      isInstantiated = false
      for (intf <- interfaces) {
        intf.removeInstantiatedSubclass(this)
        for (methodName <- allMethods().keys)
          intf.tagDynamicCallersOf(methodName)
      }
    }

    /** UPDATE PASS ONLY. */
    def walkForChanges(getLinkedClass: ClassName => LinkedClass,
        parentMethodAttributeChanges: Set[MethodName]): Unit = {

      val linkedClass = getLinkedClass(className)

      val (addedMethods, changedMethods, deletedMethods) =
        updateWith(linkedClass)

      fields = linkedClass.fields
      fieldsRead = linkedClass.fieldsRead

      val oldInterfaces = interfaces
      val newInterfaces = linkedClass.ancestors.map(getInterface).toSet
      interfaces = newInterfaces

      val methodAttributeChanges =
        (parentMethodAttributeChanges -- methods.keys ++
            addedMethods ++ changedMethods ++ deletedMethods)

      // Tag callers with dynamic calls
      val wasInstantiated = isInstantiated
      isInstantiated = linkedClass.hasInstances
      assert(!(wasInstantiated && !isInstantiated),
          "(wasInstantiated && !isInstantiated) should have been handled "+
          "during deletion phase")

      if (isInstantiated) {
        if (wasInstantiated) {
          val existingInterfaces = oldInterfaces.intersect(newInterfaces)
          for {
            intf <- existingInterfaces
            methodName <- methodAttributeChanges
          } {
            intf.tagDynamicCallersOf(methodName)
          }
          if (newInterfaces.size != oldInterfaces.size ||
              newInterfaces.size != existingInterfaces.size) {
            val allMethodNames = allMethods().keys
            for {
              intf <- oldInterfaces ++ newInterfaces -- existingInterfaces
              methodName <- allMethodNames
            } {
              intf.tagDynamicCallersOf(methodName)
            }
          }
        } else {
          val allMethodNames = allMethods().keys
          for (intf <- interfaces) {
            intf.addInstantiatedSubclass(this)
            for (methodName <- allMethodNames)
              intf.tagDynamicCallersOf(methodName)
          }
        }
      }

      // Tag callers with static calls
      for (methodName <- methodAttributeChanges)
        myInterface.tagStaticCallersOf(namespace, methodName)

      // Elidable constructors
      elidableConstructorsInfo = computeElidableConstructorsInfo(linkedClass)

      // Inlineable field bodies
      val newInlineableFieldBodies = computeInlineableFieldBodies(linkedClass)
      if (inlineableFieldBodies != newInlineableFieldBodies) {
        inlineableFieldBodies = newInlineableFieldBodies
        inlineableFieldBodiesAskers.forEachKey(Long.MaxValue, _.tag())
        inlineableFieldBodiesAskers.clear()
      }

      // Inlineable class
      if (updateTryNewInlineable(linkedClass)) {
        for (method <- methods.values; if method.methodName.isConstructor)
          myInterface.tagStaticCallersOf(namespace, method.methodName)
      }

      // Recurse in subclasses
      collOps.foreach(subclasses) { cls =>
        cls.walkForChanges(getLinkedClass, methodAttributeChanges)
      }
    }

    /** ELIDABLE CTORS PASS ONLY. */
    def setHasElidableConstructors(): Unit = {
      import ElidableConstructorsInfo._

      val newHasElidableConstructors = elidableConstructorsInfo == AcyclicElidable

      if (hasElidableConstructors != newHasElidableConstructors) {
        hasElidableConstructors = newHasElidableConstructors
        hasElidableConstructorsAskers.forEachKey(Long.MaxValue, _.tag())
        hasElidableConstructorsAskers.clear()
      }

      // Release memory that we won't use anymore
      if (!newHasElidableConstructors)
        elidableConstructorsInfo = NotElidable // get rid of DependentOn
      elidableConstructorsDependents.clear() // also resets state for next run
    }

    /** UPDATE PASS ONLY. */
    def walkForAdditions(
        getNewChildren: ClassName => collOps.ParIterable[LinkedClass]): Unit = {

      val subclassAcc = collOps.prepAdd(subclasses)

      collOps.foreach(getNewChildren(className)) { linkedClass =>
        val cls = new Class(Some(this), linkedClass)
        collOps.add(subclassAcc, cls)
        classes.put(linkedClass.className, cls)
        cls.walkForAdditions(getNewChildren)
      }

      subclasses = collOps.finishAdd(subclassAcc)
    }

    def askHasElidableConstructors(asker: Processable): Boolean = {
      hasElidableConstructorsAskers.put(asker, ())
      asker.registerTo(this)
      hasElidableConstructors
    }

    def askInlineableFieldBodies(asker: Processable): OptimizerCore.InlineableFieldBodies = {
      inlineableFieldBodiesAskers.put(asker, ())

      if (inlineableFieldBodies.isEmpty) {
        // Avoid asking for `hasInlineableConstructors`; we always get here for non-ModuleClass'es
        asker.registerTo(this)
        inlineableFieldBodies
      } else {
        // No need for asker.registerTo(this) in this branch; it is done anyway in askHasElidableConstructors
        if (askHasElidableConstructors(asker))
          inlineableFieldBodies
        else
          OptimizerCore.InlineableFieldBodies.Empty
      }
    }

    /** UPDATE PASS ONLY. */
    private def computeElidableConstructorsInfo(linkedClass: LinkedClass): ElidableConstructorsInfo = {
      import ElidableConstructorsInfo._

      if (isAdHocElidableConstructors(className)) {
        AcyclicElidable
      } else {
        // It's OK to look at the superClass like this because it will always be updated before myself
        var result = superClass.fold[ElidableConstructorsInfo](AcyclicElidable)(_.elidableConstructorsInfo)

        if (result == NotElidable) {
          // fast path
          result
        } else {
          val ctorIterator = myInterface.staticLike(MemberNamespace.Constructor).methods.valuesIterator
          while (result != NotElidable && ctorIterator.hasNext) {
            result = result.mergeWith(computeCtorElidableInfo(ctorIterator.next()))
          }
          result
        }
      }
    }

    /** UPDATE PASS ONLY. */
    private def computeInlineableFieldBodies(
        linkedClass: LinkedClass): OptimizerCore.InlineableFieldBodies = {
      import OptimizerCore.InlineableFieldBodies

      if (linkedClass.kind != ClassKind.ModuleClass) {
        InlineableFieldBodies.Empty
      } else {
        myInterface.staticLike(MemberNamespace.Constructor).methods.get(NoArgConstructorName) match {
          case None =>
            InlineableFieldBodies.Empty

          case Some(ctor) =>
            val initFieldBodies = for {
              fieldDef <- computeAllInstanceFieldDefs()
              if !fieldDef.flags.isMutable
            } yield {
              // the zero value is always a Literal because the ftpe is not a RecordType
              val zeroValue = zeroOf(fieldDef.ftpe)(NoPosition).asInstanceOf[Literal]
              fieldDef.name.name -> FieldBody.Literal(zeroValue)
            }

            if (initFieldBodies.isEmpty) {
              // fast path
              InlineableFieldBodies.Empty
            } else {
              val finalFieldBodies = interpretConstructor(ctor, initFieldBodies.toMap, Nil)
              new InlineableFieldBodies(finalFieldBodies)
            }
        }
      }
    }

    /** UPDATE PASS ONLY. */
    def updateTryNewInlineable(linkedClass: LinkedClass): Boolean = {
      val oldTryNewInlineable = tryNewInlineable

      tryNewInlineable = if (!linkedClass.optimizerHints.inline) {
        None
      } else {
        val allFields = computeAllInstanceFieldDefs()
        Some(new OptimizerCore.InlineableClassStructure(allFields))
      }

      tryNewInlineable != oldTryNewInlineable
    }

    /** UPDATE PASS ONLY, used by `computeInlineableFieldBodies` and `updateTryNewInlineable`. */
    private def computeAllInstanceFieldDefs(): List[FieldDef] = {
      for {
        parent <- reverseParentChain
        anyField <- parent.fields
        if !anyField.flags.namespace.isStatic
        // non-JS class may only contain FieldDefs (no JSFieldDef)
        field = anyField.asInstanceOf[FieldDef]
        if parent.fieldsRead.contains(field.name.name)
      } yield {
        field
      }
    }

    /** UPDATE PASS ONLY. */
    private[this] def setupAfterCreation(linkedClass: LinkedClass): Unit = {
      if (batchMode) {
        if (isInstantiated) {
          /* Only add the class to all its ancestor interfaces */
          for (intf <- interfaces)
            intf.addInstantiatedSubclass(this)
        }
      } else {
        val allMethodNames = allMethods().keys

        if (isInstantiated) {
          /* Add the class to all its ancestor interfaces + notify all callers
           * of any of the methods.
           * TODO: be more selective on methods that are notified: it is not
           * necessary to modify callers of methods defined in a parent class
           * that already existed in the previous run.
           */
          for (intf <- interfaces) {
            intf.addInstantiatedSubclass(this)
            for (methodName <- allMethodNames)
              intf.tagDynamicCallersOf(methodName)
          }
        }

        /* Tag static callers because the class could have been *moved*,
         * not just added.
         */
        for (methodName <- allMethodNames)
          myInterface.tagStaticCallersOf(namespace, methodName)
      }

      updateTryNewInlineable(linkedClass)
    }

    /** UPDATE PASS ONLY. */
    private def computeCtorElidableInfo(impl: MethodImpl): ElidableConstructorsInfo = {
      /* Dependencies on other classes to have acyclic elidable constructors
       * It is possible for the enclosing class name to be added to this set,
       * if the constructor depends on its own class. In that case, the
       * analysis will naturally treat it as a cycle and will conclude that the
       * class does not have elidable constructors.
       */
      val dependenciesBuilder = Set.newBuilder[ClassName]

      // Dependencies on certain methods to be getters
      val getterDependenciesBuilder = Set.newBuilder[(ClassName, MethodName)]

      def isTriviallySideEffectFree(tree: Tree): Boolean = tree match {
        case _:VarRef | _:Literal | _:This | _:Skip =>
          true

        case Closure(_, _, _, _, _, captureValues) =>
          captureValues.forall(isTriviallySideEffectFree(_))
        case TypedClosure(_, _, _, _, captureValues) =>
          captureValues.forall(isTriviallySideEffectFree(_))

        case GetClass(expr) =>
          config.coreSpec.semantics.nullPointers == CheckedBehavior.Unchecked &&
          isTriviallySideEffectFree(expr)

        case New(className, _, args) =>
          dependenciesBuilder += className
          args.forall(isTriviallySideEffectFree(_))

        case LoadModule(className) =>
          dependenciesBuilder += className
          true

        case Select(LoadModule(className), _) =>
          /* If the given module can be loaded without cycles, it is guaranteed
           * to be non-null, and therefore the Select is side-effect-free.
           */
          dependenciesBuilder += className
          true

        case Select(This(), _) =>
          true

        case Apply(_, LoadModule(className), MethodIdent(methodName), Nil)
            if !methodName.isReflectiveProxy =>
          // For a getter-like call, we need the method to actually be a getter.
          dependenciesBuilder += className
          getterDependenciesBuilder += ((className, methodName))
          true

        case Apply(_, This(), MethodIdent(methodName), Nil)
            if !methodName.isReflectiveProxy =>
          getterDependenciesBuilder += ((className, methodName))
          true

        case _ =>
          false
      }

      def isElidableStat(tree: Tree): Boolean = tree match {
        case Block(stats)                   => stats.forall(isElidableStat)
        case Assign(Select(This(), _), rhs) => isTriviallySideEffectFree(rhs)

        // Mixin constructor -- test whether its body is entirely empty
        case ApplyStatically(flags, This(), className, methodName, Nil)
            if !flags.isPrivate && !classes.containsKey(className) =>
          // Since className is not in classes, it must be a default method call.
          val container =
            getInterface(className).staticLike(MemberNamespace.Public)
          container.methods.get(methodName.name) exists { methodDef =>
            methodDef.originalDef.body exists {
              case Skip() => true
              case _      => false
            }
          }

        /* Delegation to another constructor (super or in the same class)
         *
         * - for super constructor calls, we have already checked before getting
         *   here that the super class has elidable constructors, so by
         *   construction they are elidable and we do not need to test them
         * - for other constructors in the same class, we will collectively
         *   treat them as all-elidable or non-elidable; therefore, we do not
         *   need to check them either at this point.
         *
         * We only need to check the arguments to the constructor, not their
         * bodies.
         */
        case ApplyStatically(flags, This(), _, _, args) if flags.isConstructor =>
          args.forall(isTriviallySideEffectFree)

        case StoreModule() => true
        case _             => isTriviallySideEffectFree(tree)
      }

      impl.originalDef.body.fold {
        throw new AssertionError("Constructor cannot be abstract")
      } { body =>
        if (isElidableStat(body)) {
          val dependencies = dependenciesBuilder.result()
          val getterDependencies = getterDependenciesBuilder.result()
          if (dependencies.isEmpty && getterDependencies.isEmpty)
            ElidableConstructorsInfo.AcyclicElidable
          else
            ElidableConstructorsInfo.DependentOn(dependencies, getterDependencies)
        } else {
          ElidableConstructorsInfo.NotElidable
        }
      }
    }

    /** UPDATE PASS ONLY. */
    private def interpretConstructor(impl: MethodImpl,
        fieldBodies: Map[FieldName, FieldBody],
        paramBodies: List[Option[FieldBody]]): Map[FieldName, FieldBody] = {

      /* This method performs a kind of abstract intepretation of the given
       * given constructor `impl`. It *assumes* that the enclosing class ends
       * up having elidable constructors. If that is not the case, the
       * computation must not crash but its result can be arbitrary.
       */

      type FieldBodyMap = Map[FieldName, FieldBody]
      type ParamBodyMap = Map[LocalName, FieldBody]

      def interpretSelfGetter(methodName: MethodName,
          fieldBodies: FieldBodyMap): Option[FieldBody] = {
        methods.get(methodName).flatMap { impl =>
          impl.originalDef.body match {
            case Some(Select(This(), FieldIdent(fieldName))) =>
              fieldBodies.get(fieldName)

            case _ =>
              /* If we get here, it means the class won't have elidable
               * constructors, and therefore we can return arbitrary values
               * from the whole method, so we don't care.
               */
              None
          }
        }
      }

      def interpretExpr(tree: Tree, fieldBodies: FieldBodyMap,
          paramBodies: ParamBodyMap): Option[FieldBody] = {
        tree match {
          case lit: Literal =>
            Some(FieldBody.Literal(lit))

          case VarRef(LocalIdent(valName)) =>
            paramBodies.get(valName)

          case LoadModule(moduleClassName) =>
            Some(FieldBody.LoadModule(moduleClassName, tree.pos))

          case Select(qual @ LoadModule(moduleClassName), FieldIdent(fieldName)) =>
            val moduleBody = FieldBody.LoadModule(moduleClassName, qual.pos)
            Some(FieldBody.ModuleSelect(moduleBody, fieldName, tree.tpe, tree.pos))

          case Select(This(), FieldIdent(fieldName)) =>
            fieldBodies.get(fieldName)

          case Apply(_, receiver @ LoadModule(moduleClassName), MethodIdent(methodName), Nil)
              if !methodName.isReflectiveProxy =>
            val moduleBody = FieldBody.LoadModule(moduleClassName, receiver.pos)
            Some(FieldBody.ModuleGetter(moduleBody, methodName, tree.tpe, tree.pos))

          case Apply(_, This(), MethodIdent(methodName), Nil)
              if !methodName.isReflectiveProxy =>
            interpretSelfGetter(methodName, fieldBodies)

          case _ =>
            None
        }
      }

      def interpretBody(body: Tree, fieldBodies: FieldBodyMap,
          paramBodies: ParamBodyMap): FieldBodyMap = {
        body match {
          case Block(stats) =>
            stats.foldLeft(fieldBodies) { (prev, stat) =>
              interpretBody(stat, prev, paramBodies)
            }

          case Skip() =>
            fieldBodies

          case Assign(Select(This(), FieldIdent(fieldName)), rhs) =>
            if (!fieldBodies.contains(fieldName)) {
              // It is a mutable field or it is dce'ed as never read, don't track it
              fieldBodies
            } else {
              interpretExpr(rhs, fieldBodies, paramBodies) match {
                case Some(newFieldBody) =>
                  fieldBodies.updated(fieldName, newFieldBody)
                case None =>
                  // Unknown value; don't track it anymore
                  fieldBodies - fieldName
              }
            }

          // Mixin constructor -- assume it is empty
          case ApplyStatically(flags, This(), className, methodName, Nil)
              if !flags.isPrivate && !classes.containsKey(className) =>
            fieldBodies

          // Delegation to another constructor
          case ApplyStatically(flags, This(), className, MethodIdent(methodName), args) if flags.isConstructor =>
            val argBodies = args.map(interpretExpr(_, fieldBodies, paramBodies))
            val ctorImpl = getInterface(className)
              .staticLike(MemberNamespace.Constructor)
              .methods(methodName)
            interpretConstructor(ctorImpl, fieldBodies, argBodies)

          case _ =>
            /* Other statements cannot affect the eventual fieldBodies
             * (assuming the class ends up having elidable constructors, as always).
             */
            fieldBodies
        }
      }

      impl.originalDef.body.fold {
        throw new AssertionError(s"Constructor $impl cannot be abstract")
      } { body =>
        val paramBodiesMap: ParamBodyMap =
          impl.originalDef.args.zip(paramBodies).collect {
            case (paramDef, Some(paramBody)) => paramDef.name.name -> paramBody
          }.toMap

        interpretBody(body, fieldBodies, paramBodiesMap)
      }
    }

    /** All the methods of this class, including inherited ones.
     *  It has () so we remember this is an expensive operation.
     *  UPDATE PASS ONLY.
     */
    def allMethods(): scala.collection.Map[MethodName, MethodImpl] = {
      val result = mutable.Map.empty[MethodName, MethodImpl]
      for (parent <- reverseParentChain)
        result ++= parent.methods
      result
    }

    /** BOTH PASSES. */
    @tailrec
    final def lookupMethod(methodName: MethodName): Option[MethodImpl] = {
      methods.get(methodName) match {
        case Some(impl) => Some(impl)
        case none =>
          superClass match {
            case Some(p) => p.lookupMethod(methodName)
            case none    => None
          }
      }
    }

    def unregisterDependee(dependee: Processable): Unit = {
      hasElidableConstructorsAskers.remove(dependee)
      inlineableFieldBodiesAskers.remove(dependee)
    }
  }

  /** Namespace for static members of a class. */
  private final class StaticLikeNamespace(linkedClass: LinkedClass,
      myInterface: InterfaceType, namespace: MemberNamespace)
      extends MethodContainer(linkedClass, myInterface, namespace) {

    /** BOTH PASSES. */
    final def lookupMethod(methodName: MethodName): Option[MethodImpl] =
      methods.get(methodName)
  }

  private sealed abstract class JSMethodContainer {
    def untrackedJSClassCaptures: List[ParamDef]
    def untrackedThisType(namespace: MemberNamespace): Type
  }

  private final class JSClassMethodContainer(linkedClass: LinkedClass,
      val myInterface: InterfaceType) extends JSMethodContainer {

    val className: ClassName = linkedClass.className

    private[this] val exportedMembers = mutable.ArrayBuffer.empty[JSMethodImpl]
    private[this] var jsConstructorDef: Option[JSCtorImpl] = None
    private[this] var _jsClassCaptures: List[ParamDef] = Nil

    updateWith(linkedClass)

    /** JS class captures
     *
     *  A similar argument applies here than for
     *  [[InterfaceType#untrackedThisType]]: The captures are merely a
     *  convenience for the optimizer's environment: Any real change of usage
     *  also necessarily changes the body of the method.
     */
    def untrackedJSClassCaptures: List[ParamDef] = _jsClassCaptures

    def untrackedThisType(namespace: MemberNamespace): Type =
      if (namespace.isStatic) NoType
      else myInterface.untrackedInstanceThisType

    def updateWith(linkedClass: LinkedClass): Unit = {
      _jsClassCaptures = linkedClass.jsClassCaptures.getOrElse(Nil)
      updateExportedMembers(linkedClass.exportedMembers)
      updateJSConstructorDef(linkedClass.jsConstructorDef)
    }

    private def updateExportedMembers(
        newExportedMembers: List[JSMethodPropDef]): Unit = {
      val newLen = newExportedMembers.length
      val oldLen = exportedMembers.length

      if (newLen > oldLen) {
        exportedMembers.sizeHint(newLen)
        for (i <- oldLen until newLen)
          exportedMembers += new JSMethodImpl(this, i)
      } else if (newLen < oldLen) {
        for (i <- newLen until oldLen)
          exportedMembers(i).delete()
        exportedMembers.dropRightInPlace(oldLen - newLen)
      }

      for {
        (method, methodIdx) <- newExportedMembers.zipWithIndex
      } {
        exportedMembers(methodIdx).updateWith(method)
      }
    }

    private def updateJSConstructorDef(
        newJSConstructorDef: Option[JSConstructorDef]): Unit = {

      newJSConstructorDef.fold {
        jsConstructorDef.foreach(_.delete())
        jsConstructorDef = None
      } { newJSConstructorDef =>
        if (jsConstructorDef.isEmpty) {
          jsConstructorDef = Some(new JSCtorImpl(this))
        }

        jsConstructorDef.get.updateWith(newJSConstructorDef)
      }
    }

    def optimizedExportedMembers(): List[JSMethodPropDef] =
      exportedMembers.map(_.optimizedDef).toList

    def optimizedJSConstructorDef(): Option[JSConstructorDef] =
      jsConstructorDef.map(_.optimizedDef)
  }

  private final class JSTopLevelMethodContainer extends JSMethodContainer {

    private[this] var methods = Map.empty[(String, String), (JSMethodImpl, Position)]

    val untrackedJSClassCaptures: List[ParamDef] = Nil
    def untrackedThisType(namespace: MemberNamespace): Type = NoType

    def updateWith(topLevelExports: List[LinkedTopLevelExport]): Unit = {
      val newMethods = topLevelExports.map(_.tree).collect {
        case m: TopLevelMethodExportDef =>
          val key = (m.moduleID, m.topLevelExportName)
          val impl = methods.get(key).fold(new JSMethodImpl(this, key))(_._1)
          impl.updateWith(m.methodDef)
          key -> (impl, m.pos)
      }.toMap

      methods
        .withFilter(e => !newMethods.contains(e._1))
        .foreach(_._2._1.delete())

      methods = newMethods
    }

    def optimizedMethod(moduleID: String, name: String): TopLevelMethodExportDef = {
      val (impl, pos) = methods((moduleID, name))
      val newMethod = impl.optimizedDef.asInstanceOf[JSMethodDef]
      TopLevelMethodExportDef(moduleID, newMethod)(pos)
    }
  }

  /** Thing from which a [[MethodImpl]] can unregister itself from. */
  private trait Unregisterable {
    /** UPDATE PASS ONLY. */
    def unregisterDependee(dependee: Processable): Unit
  }

  /** Type of a class or interface.
   *
   *  There exists exactly one instance of this per LinkedClass.
   *
   *  Fully concurrency safe unless otherwise noted.
   */
  private final class InterfaceType(linkedClass: LinkedClass) extends Unregisterable {

    val className: ClassName = linkedClass.className

    private type MethodCallers =
      ConcurrentHashMap[MethodName, ConcurrentHashMap[Processable, Unit]]

    private val ancestorsAskers = new ConcurrentHashMap[Processable, Unit]
    private val dynamicCallers: MethodCallers = new ConcurrentHashMap

    // ArrayBuffer to avoid need for ClassTag[collOps.Map[_, _]]
    private val staticCallers =
      mutable.ArrayBuffer.fill[MethodCallers](MemberNamespace.Count)(new ConcurrentHashMap)

    private val jsNativeImportsAskers = new ConcurrentHashMap[Processable, Unit]
    private val fieldsReadAskers = new ConcurrentHashMap[Processable, Unit]

    private var _ancestors: List[ClassName] = linkedClass.ancestors

    private val _instantiatedSubclasses = new ConcurrentHashMap[Class, Unit]

    private val staticLikes: Array[StaticLikeNamespace] = {
      Array.tabulate(MemberNamespace.Count) { ord =>
        new StaticLikeNamespace(linkedClass, this, MemberNamespace.fromOrdinal(ord))
      }
    }

    private val jsMethodContainer = new JSClassMethodContainer(linkedClass, this)

    /* For now, we track all JS native imports together (the class itself and native members).
     *
     * This is more to avoid unnecessary tracking than due to an intrinsic reason.
     */

    private type JSNativeImports =
      (Option[JSNativeLoadSpec.Import], Map[MethodName, JSNativeLoadSpec.Import])

    private var jsNativeImports: JSNativeImports =
      computeJSNativeImports(linkedClass)

    /* Similar comment than for JS native imports:
     * We track read state of all fields together to avoid too much tracking.
     */

    private var fieldsRead: Set[FieldName] = linkedClass.fieldsRead
    private var staticFieldsRead: Set[FieldName] = linkedClass.staticFieldsRead

    /** The type of instances of this interface.
     *
     *  Offered via untracked accessor since its only usage is in the
     *  environment of the Optimizer.
     *
     *  However, this is merely a convenience: If the this type changes
     *  and a method body relies on it, the method body itself must change,
     *  because the type of the This() tree must change.
     *
     *  Therefore, any tracking would be unnecessarily duplicate.
     */
    def untrackedInstanceThisType: Type = _instanceThisType

    private var _instanceThisType = computeInstanceThisType(linkedClass)

    override def toString(): String =
      s"intf ${className.nameString}"

    /** PROCESS PASS ONLY. */
    def askDynamicCallTargets(methodName: MethodName,
        asker: Processable): List[MethodImpl] = {
      dynamicCallers
        .computeIfAbsent(methodName, _ => new ConcurrentHashMap())
        .put(asker, ())
      asker.registerTo(this)

      val res = mutable.Set.empty[MethodImpl]
      _instantiatedSubclasses.forEachKey(Long.MaxValue, _.lookupMethod(methodName).foreach(res += _))
      res.toList
    }

    /** PROCESS PASS ONLY. */
    def askStaticCallTarget(namespace: MemberNamespace, methodName: MethodName,
        asker: Processable): MethodImpl = {
      staticCallers(namespace.ordinal)
        .computeIfAbsent(methodName, _ => new ConcurrentHashMap())
        .put(asker, ())
      asker.registerTo(this)

      def inStaticsLike = staticLike(namespace)

      val container =
        if (namespace != MemberNamespace.Public) inStaticsLike
        else classOrElse(className, inStaticsLike)

      // Method must exist, otherwise it's a bug / invalid IR.
      container.lookupMethod(methodName).getOrElse {
        throw new AssertionError(s"could not find method $className.$methodName")
      }
    }

    /** UPDATE PASS ONLY. */
    def addInstantiatedSubclass(x: Class): Unit =
      _instantiatedSubclasses.put(x, ())

    /** UPDATE PASS ONLY. */
    def removeInstantiatedSubclass(x: Class): Unit =
      _instantiatedSubclasses.remove(x)

    /** PROCESS PASS ONLY. */
    def askAncestors(asker: Processable): List[ClassName] = {
      ancestorsAskers.put(asker, ())
      asker.registerTo(this)
      _ancestors
    }

    /** PROCESS PASS ONLY. Concurrency safe except with [[updateWith]]. */
    def askJSNativeImport(asker: Processable): Option[JSNativeLoadSpec.Import] = {
      jsNativeImportsAskers.put(asker, ())
      asker.registerTo(this)
      jsNativeImports._1
    }

    /** PROCESS PASS ONLY. Concurrency safe except with [[updateWith]]. */
    def askJSNativeImport(methodName: MethodName,
        asker: Processable): Option[JSNativeLoadSpec.Import] = {
      jsNativeImportsAskers.put(asker, ())
      asker.registerTo(this)
      jsNativeImports._2.get(methodName)
    }

    def askFieldRead(name: FieldName, asker: Processable): Boolean = {
      fieldsReadAskers.put(asker, ())
      asker.registerTo(this)
      fieldsRead.contains(name)
    }

    def askStaticFieldRead(name: FieldName, asker: Processable): Boolean = {
      fieldsReadAskers.put(asker, ())
      asker.registerTo(this)
      staticFieldsRead.contains(name)
    }

    @inline
    def staticLike(namespace: MemberNamespace): StaticLikeNamespace =
      staticLikes(namespace.ordinal)

    def optimizedExportedMembers(): List[JSMethodPropDef] =
      jsMethodContainer.optimizedExportedMembers()

    def optimizedJSConstructorDef(): Option[JSConstructorDef] =
      jsMethodContainer.optimizedJSConstructorDef()

    /** UPDATE PASS ONLY. Not concurrency safe. */
    def updateWith(linkedClass: LinkedClass): Unit = {
      // Update ancestors
      if (linkedClass.ancestors != _ancestors) {
        _ancestors = linkedClass.ancestors
        ancestorsAskers.forEachKey(Long.MaxValue, _.tag())
        ancestorsAskers.clear()
      }

      // Update jsNativeImports
      val newJSNativeImports = computeJSNativeImports(linkedClass)
      if (jsNativeImports != newJSNativeImports) {
        jsNativeImports = newJSNativeImports
        jsNativeImportsAskers.forEachKey(Long.MaxValue, _.tag())
        jsNativeImportsAskers.clear()
      }

      // Update fields read.
      if (fieldsRead != linkedClass.fieldsRead ||
          staticFieldsRead != linkedClass.staticFieldsRead) {
        fieldsRead = linkedClass.fieldsRead
        staticFieldsRead = linkedClass.staticFieldsRead
        fieldsReadAskers.forEachKey(Long.MaxValue, _.tag())
        fieldsReadAskers.clear()
      }

      // Update static likes
      for (staticLike <- staticLikes) {
        val (_, changed, _) = staticLike.updateWith(linkedClass)
        for (method <- changed) {
          this.tagStaticCallersOf(staticLike.namespace, method)
        }
      }

      _instanceThisType = computeInstanceThisType(linkedClass)

      jsMethodContainer.updateWith(linkedClass)
    }

    /** UPDATE PASS ONLY. */
    def delete(): Unit = {
      // Mark all static like methods as deleted.
      staticLikes.foreach(_.methods.values.foreach(_.delete()))
    }

    /** Tag the dynamic-callers of an instance method.
     *  UPDATE PASS ONLY.
     */
    def tagDynamicCallersOf(methodName: MethodName): Unit = {
      val callers = dynamicCallers.remove(methodName)
      if (callers != null)
        callers.forEachKey(Long.MaxValue, _.tag())
    }

    /** Tag the static-callers of an instance method.
     *  UPDATE PASS ONLY.
     */
    def tagStaticCallersOf(namespace: MemberNamespace,
        methodName: MethodName): Unit = {
      val callers = staticCallers(namespace.ordinal).remove(methodName)
      if (callers != null)
        callers.forEachKey(Long.MaxValue, _.tag())
    }

    /** UPDATE PASS ONLY. */
    def unregisterDependee(dependee: Processable): Unit = {
      ancestorsAskers.remove(dependee)
      dynamicCallers.forEachValue(Long.MaxValue, _.remove(dependee))
      staticCallers.foreach(_.forEachValue(Long.MaxValue, _.remove(dependee)))
      jsNativeImportsAskers.remove(dependee)
    }

    private def computeJSNativeImports(linkedClass: LinkedClass): JSNativeImports = {
      def maybeImport(spec: JSNativeLoadSpec): Option[JSNativeLoadSpec.Import] = spec match {
        case i: JSNativeLoadSpec.Import =>
          Some(i)

        case JSNativeLoadSpec.ImportWithGlobalFallback(i, _) =>
          if (config.coreSpec.moduleKind != ModuleKind.NoModule) Some(i)
          else None

        case _: JSNativeLoadSpec.Global =>
          None
      }

      val clazz = linkedClass.jsNativeLoadSpec.flatMap(maybeImport(_))
      val nativeMembers = for {
        member <- linkedClass.jsNativeMembers
        jsImport <- maybeImport(member.jsNativeLoadSpec)
      } yield {
        member.name.name -> jsImport
      }

      (clazz, nativeMembers.toMap)
    }

    private def computeInstanceThisType(linkedClass: LinkedClass): Type = {
      if (linkedClass.kind.isJSType) AnyType
      else if (linkedClass.kind == ClassKind.HijackedClass) BoxedClassToPrimType(className)
      else ClassType(className)
    }
  }

  /** A thing that can be tagged for reprocessing and then reprocessed. */
  private abstract class Processable {
    type Def >: scala.Null <: VersionedMemberDef

    private[this] val registeredTo = new ConcurrentHashMap[Unregisterable, Unit]
    private[this] val tagged = new AtomicBoolean(false)
    private[this] var _deleted: Boolean = false

    private[this] var lastInVersion: Version = Version.Unversioned
    private[this] var lastOutVersion: Int = 0

    private[this] var _originalDef: Def = _
    private[this] var _optimizedDef: Def = _

    protected def doProcess(newVersion: Version): Def

    final def deleted: Boolean = _deleted

    final def originalDef: Def = _originalDef
    final def optimizedDef: Def = _optimizedDef

    /** PROCESS PASS ONLY. */
    final def process(): Unit = {
      if (!_deleted) {
        lastOutVersion += 1
        _optimizedDef = doProcess(Version.fromInt(lastOutVersion))
        tagged.set(false)
      }
    }

    /** Returns true if the method changed */
    protected def updateDef(methodDef: Def): Boolean = {
      assert(!deleted, "updateDef() called on a deleted method")

      if (lastInVersion.sameVersion(methodDef.version)) {
        false
      } else {
        lastInVersion = methodDef.version
        _originalDef = methodDef
        _optimizedDef = null
        tag()
        true
      }
    }

    private def unregisterFromEverywhere(): Unit = {
      registeredTo.forEachKey(Long.MaxValue, _.unregisterDependee(this))
      registeredTo.clear()
    }

    /** UPDATE PASS ONLY. Not concurrency safe on same instance. */
    final def delete(): Unit = {
      assert(!_deleted, "delete() called twice")
      _deleted = true
      if (protectTag())
        unregisterFromEverywhere()
    }

    /** Concurrency safe with itself and [[delete]] on the same instance
     *
     *  [[tag]] can be called concurrently with [[delete]] when methods in
     *  traits/classes are updated.
     *
     *  UPDATE PASS ONLY.
     */
    final def tag(): Unit = {
      if (protectTag()) {
        collOps.add(methodsToProcess, this)
        unregisterFromEverywhere()
      }
    }

    /** PROCESS PASS ONLY. */
    final def registerTo(unregisterable: Unregisterable): Unit =
      registeredTo.put(unregisterable, ())

    /** Tag this method and return true iff it wasn't tagged before.
     *  UPDATE PASS ONLY.
     */
    private def protectTag(): Boolean = !tagged.getAndSet(true)
  }

  /** A method implementation.
   *  It must be concrete, and belong either to a [[IncOptimizer.Class]] or a
   *  [[IncOptimizer.StaticsNamespace]].
   *
   *  A single instance is **not** concurrency safe (unless otherwise noted in
   *  a method comment). However, the global state modifications are
   *  concurrency safe.
   */
  private final class MethodImpl(owner: MethodContainer,
      val methodName: MethodName)
      extends Processable with OptimizerCore.AbstractMethodID with Unregisterable {

    type Def = MethodDef

    private val bodyAskers = new ConcurrentHashMap[Processable, Unit]

    var attributes: OptimizerCore.MethodAttributes = _

    def enclosingClassName: ClassName = owner.className

    override def toString(): String =
      s"$owner.${methodName.nameString}"

    /** PROCESS PASS ONLY. */
    def askBody(asker: Processable): MethodDef = {
      bodyAskers.put(asker, ())
      asker.registerTo(this)
      originalDef
    }

    /** UPDATE PASS ONLY. */
    def tagBodyAskers(): Unit = {
      bodyAskers.forEachKey(Long.MaxValue, _.tag())
      bodyAskers.clear()
    }

    /** UPDATE PASS ONLY. */
    def unregisterDependee(dependee: Processable): Unit =
      bodyAskers.remove(dependee)

    /** Returns true if the method's attributes changed.
     *  Attributes are whether it is inlineable, and whether it is a trait
     *  impl forwarder. Basically this is what is declared in
     *  `OptimizerCore.AbstractMethodID`.
     *  In the process, tags all the body askers if the body changes.
     *  UPDATE PASS ONLY. Not concurrency safe on same instance.
     */
    def updateWith(methodDef: MethodDef): Boolean = {
      val changed = updateDef(methodDef)
      if (changed) {
        tagBodyAskers()

        val oldAttributes = attributes
        attributes = OptimizerCore.MethodAttributes.compute(enclosingClassName, methodDef)
        attributes != oldAttributes
      } else {
        false
      }
    }

    /** PROCESS PASS ONLY. */
    protected def doProcess(newVersion: Version): MethodDef = {
      val MethodDef(static, name, originalName, params, resultType, optBody) =
        originalDef
      val body = optBody.getOrElse {
        throw new AssertionError("Methods to optimize must be concrete")
      }

      val (newParams, newBody) = new Optimizer(this, Some(this), this.toString()).optimize(
          owner.untrackedThisType, params, jsClassCaptures = Nil,
          resultType, body, isNoArgCtor = name.name == NoArgConstructorName)

      MethodDef(static, name, originalName,
          newParams, resultType, Some(newBody))(
          originalDef.optimizerHints, newVersion)(originalDef.pos)
    }
  }

  private final class JSMethodImpl(owner: JSMethodContainer, id: Any) extends Processable {

    type Def = JSMethodPropDef

    override def toString(): String =
      s"$owner[$id]"

    def updateWith(linkedMethod: JSMethodPropDef): Unit =
      updateDef(linkedMethod)

    protected def doProcess(newVersion: Version): JSMethodPropDef = {
      originalDef match {
        case originalDef @ JSMethodDef(flags, name, params, restParam, body) =>
          val thisType = owner.untrackedThisType(flags.namespace)

          val (newParamsAndRest, newBody) = new Optimizer(this, None, this.toString()).optimize(
            thisType, params ++ restParam.toList, owner.untrackedJSClassCaptures,
            AnyType, body, isNoArgCtor = false)

          val (newParams, newRestParam) =
            if (restParam.isDefined) (newParamsAndRest.init, Some(newParamsAndRest.last))
            else (newParamsAndRest, None)

          JSMethodDef(flags, name, newParams, newRestParam, newBody)(
              originalDef.optimizerHints, newVersion)(originalDef.pos)

        case originalDef @ JSPropertyDef(flags, name, getterBody, setterArgAndBody) =>
          val thisType = owner.untrackedThisType(flags.namespace)
          val jsClassCaptures = owner.untrackedJSClassCaptures

          val newGetterBody = getterBody.map { body =>
            val (_, newBody) = new Optimizer(this, None, "get " + this.toString()).optimize(
                thisType, Nil, jsClassCaptures, AnyType, body, isNoArgCtor = false)
            newBody
          }

          val newSetterArgAndBody = setterArgAndBody.map { case (param, body) =>
            val (List(newParam), newBody) = new Optimizer(this, None, "set " + this.toString()).optimize(
                thisType, List(param), jsClassCaptures, AnyType, body,
                isNoArgCtor = false)
            (newParam, newBody)
          }

          JSPropertyDef(flags, name, newGetterBody, newSetterArgAndBody)(newVersion)(originalDef.pos)
      }
    }
  }

  private final class JSCtorImpl(owner: JSMethodContainer) extends Processable {

    type Def = JSConstructorDef

    override def toString(): String =
      s"$owner ctor"

    def updateWith(linkedMethod: JSConstructorDef): Unit =
      updateDef(linkedMethod)

    protected def doProcess(newVersion: Version): JSConstructorDef = {
      val JSConstructorDef(flags, params, restParam, body) = originalDef

      val thisType = owner.untrackedThisType(flags.namespace)

      val (newParamsAndRest, newRawBody) = new Optimizer(this, None, this.toString()).optimize(
          thisType, params ++ restParam.toList, owner.untrackedJSClassCaptures, AnyType,
          Block(body.allStats)(body.pos), isNoArgCtor = false)

      val (newParams, newRestParam) =
        if (restParam.isDefined) (newParamsAndRest.init, Some(newParamsAndRest.last))
        else (newParamsAndRest, None)

      val bodyStats = newRawBody match {
        case Block(stats) => stats
        case stat         => List(stat)
      }

      val (beforeSuper, superCall :: afterSuper) =
        bodyStats.span(!_.isInstanceOf[JSSuperConstructorCall])

      val newBody = JSConstructorBody(beforeSuper,
          superCall.asInstanceOf[JSSuperConstructorCall], afterSuper)(body.pos)

      JSConstructorDef(flags, newParams, newRestParam, newBody)(
          originalDef.optimizerHints, newVersion)(originalDef.pos)
    }
  }

  /** Concrete optimizer bound to types we use.
   *
   *  All methods are PROCESS PASS ONLY
   */
  private final class Optimizer(
      asker: Processable, protected val myself: Option[MethodImpl], debugID: String)
      extends OptimizerCore(config, debugID) {
    import OptimizerCore.ImportTarget

    type MethodID = MethodImpl

    protected def getMethodBody(method: MethodID): MethodDef =
      method.askBody(asker)

    /** Look up the targets of a dynamic call to an instance method. */
    protected def dynamicCall(intfName: ClassName,
        methodName: MethodName): List[MethodID] = {
      getInterface(intfName).askDynamicCallTargets(methodName, asker)
    }

    /** Look up the target of a static call to an instance method. */
    protected def staticCall(className: ClassName, namespace: MemberNamespace,
        methodName: MethodName): MethodID = {
      getInterface(className).askStaticCallTarget(namespace, methodName, asker)
    }

    protected def getAncestorsOf(intfName: ClassName): List[ClassName] =
      getInterface(intfName).askAncestors(asker)

    protected def hasElidableConstructors(className: ClassName): Boolean =
      classes.get(className).askHasElidableConstructors(asker)

    protected def inlineableFieldBodies(className: ClassName): OptimizerCore.InlineableFieldBodies = {
      val clazz = classes.get(className)
      if (clazz == null) OptimizerCore.InlineableFieldBodies.Empty
      else clazz.askInlineableFieldBodies(asker)
    }

    protected def tryNewInlineableClass(
        className: ClassName): Option[OptimizerCore.InlineableClassStructure] = {
      classes.get(className).tryNewInlineable
    }

    protected def getJSNativeImportOf(
        target: ImportTarget): Option[JSNativeLoadSpec.Import] = {
      target match {
        case ImportTarget.Class(className) =>
          getInterface(className).askJSNativeImport(asker)
        case ImportTarget.Member(className, methodName) =>
          getInterface(className).askJSNativeImport(methodName, asker)
      }
    }

    protected def isFieldRead(fieldName: FieldName): Boolean =
      getInterface(fieldName.className).askFieldRead(fieldName, asker)

    protected def isStaticFieldRead(fieldName: FieldName): Boolean =
      getInterface(fieldName.className).askStaticFieldRead(fieldName, asker)
  }

}

object IncOptimizer {
  def apply(config: CommonPhaseConfig): IncOptimizer =
    new IncOptimizer(config, SeqCollOps)

  private val isAdHocElidableConstructors: Set[ClassName] =
    Set(ClassName("scala.Predef$"), ClassName("scala.package$"))

  sealed abstract class ElidableConstructorsInfo {
    import ElidableConstructorsInfo._

    final def mergeWith(that: ElidableConstructorsInfo): ElidableConstructorsInfo = (this, that) match {
      case (DependentOn(deps1, getterDeps1), DependentOn(deps2, getterDeps2)) =>
        DependentOn(deps1 ++ deps2, getterDeps1 ++ getterDeps2)
      case (AcyclicElidable, _) =>
        that
      case (_, AcyclicElidable) =>
        this
      case _ =>
        NotElidable
    }
  }

  object ElidableConstructorsInfo {
    case object NotElidable extends ElidableConstructorsInfo

    case object AcyclicElidable extends ElidableConstructorsInfo

    final case class DependentOn(
      dependencies: Set[ClassName],
      getterDependencies: Set[(ClassName, MethodName)]
    ) extends ElidableConstructorsInfo
  }
}
