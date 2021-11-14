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

import java.util.concurrent.atomic.AtomicBoolean

import org.scalajs.ir._
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.logging._

import org.scalajs.linker._
import org.scalajs.linker.backend.emitter.LongImpl
import org.scalajs.linker.frontend.LinkingUnit
import org.scalajs.linker.standard._
import org.scalajs.linker.CollectionsCompat.MutableMapCompatOps

/** Incremental optimizer.
 *
 *  An incremental optimizer optimizes a [[LinkingUnit]]
 *  in an incremental way.
 *
 *  It maintains state between runs to do a minimal amount of work on every
 *  run, based on detecting what parts of the program must be re-optimized,
 *  and keeping optimized results from previous runs for the rest.
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
  private val classes = collOps.emptyMap[ClassName, Class]
  private val interfaces = collOps.emptyParMap[ClassName, InterfaceType]

  private var methodsToProcess = collOps.emptyAddable[MethodImpl]

  @inline
  private def getInterface(className: ClassName): InterfaceType =
    collOps.forceGet(interfaces, className)

  /** Update the incremental analyzer with a new run. */
  def update(unit: LinkingUnit, logger: Logger): LinkingUnit = {
    batchMode = objectClass == null
    logger.debug(s"Optimizer: Batch mode: $batchMode")

    logger.time("Optimizer: Incremental part") {
      /* UPDATE PASS */
      updateAndTagEverything(unit.classDefs)
    }

    logger.time("Optimizer: Optimizer part") {
      /* PROCESS PASS */
      processAllTaggedMethods(logger)
    }

    val newLinkedClasses = for (linkedClass <- unit.classDefs) yield {
      val className = linkedClass.className
      val interface = getInterface(className)

      val publicContainer = classes.get(className).getOrElse {
        /* For interfaces, we need to look at default methods.
         * For other kinds of classes, the public namespace is necessarily
         * empty.
         */
        val container = interface.staticLike(MemberNamespace.Public)
        assert(
            linkedClass.kind == ClassKind.Interface || container.methods.isEmpty,
            linkedClass.className -> linkedClass.kind)
        container
      }

      val newMethods = for (m <- linkedClass.methods) yield {
        val namespace = m.value.flags.namespace
        val container =
          if (namespace == MemberNamespace.Public) publicContainer
          else interface.staticLike(namespace)
        container.methods(m.value.methodName).optimizedMethodDef
      }

      linkedClass.optimized(methods = newMethods)
    }

    new LinkingUnit(unit.coreSpec, newLinkedClasses, unit.topLevelExports,
        unit.moduleInitializers)
  }

  /** Incremental part: update state and detect what needs to be re-optimized.
   *  UPDATE PASS ONLY. (This IS the update pass).
   */
  private def updateAndTagEverything(linkedClasses: List[LinkedClass]): Unit = {
    val neededInterfaces = collOps.emptyParMap[ClassName, LinkedClass]
    val neededClasses = collOps.emptyParMap[ClassName, LinkedClass]
    for (linkedClass <- linkedClasses) {
      collOps.put(neededInterfaces, linkedClass.className, linkedClass)

      if (linkedClass.hasInstances &&
          (linkedClass.kind.isClass || linkedClass.kind == ClassKind.HijackedClass)) {
        collOps.put(neededClasses, linkedClass.className, linkedClass)
      }
    }

    /* Remove deleted interfaces, and update existing ones.
     * We don't even have to notify callers in case of additions or removals
     * because callers have got to be invalidated by themselves.
     * Only changed methods need to trigger notifications.
     *
     * Non-batch mode only.
     */
    assert(!batchMode || collOps.isEmpty(interfaces))
    if (!batchMode) {
      collOps.retain(interfaces) { (className, interface) =>
        collOps.remove(neededInterfaces, className).fold {
          interface.delete()
          false
        } { linkedClass =>
          interface.updateWith(linkedClass)
          true
        }
      }
    }

    /* Add new interfaces.
     * Easy, we don't have to notify anyone.
     */
    collOps.valuesForeach(neededInterfaces) { linkedClass =>
      val interface = new InterfaceType(linkedClass.className)
      collOps.put(interfaces, interface.className, interface)
      interface.updateWith(linkedClass)
    }

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
        objectClass.walkClassesForDeletions(collOps.get(neededClasses, _))
      assert(objectClassStillExists, "Uh oh, java.lang.Object was deleted!")

      /* Class changes:
       * * Delete removed methods, update existing ones, add new ones
       * * Update the list of ancestors
       * * Class newly instantiated
       *
       * Non-batch mode only.
       */
      objectClass.walkForChanges(
          collOps.remove(neededClasses, _).get, Set.empty)
    }

    /* Class additions:
     * * Add new classes (including those that have moved from elsewhere).
     * In batch mode, we avoid doing notifications.
     */

    // Group children by (immediate) parent
    val newChildrenByParent = collOps.emptyAccMap[ClassName, LinkedClass]

    collOps.valuesForeach(neededClasses) { linkedClass =>
      linkedClass.superClass.fold {
        assert(batchMode, "Trying to add java.lang.Object in incremental mode")
        objectClass = new Class(None, linkedClass.className)
        classes += linkedClass.className -> objectClass
        objectClass.setupAfterCreation(linkedClass)
      } { superClassName =>
        collOps.acc(newChildrenByParent, superClassName.name, linkedClass)
      }
    }

    val getNewChildren =
      (name: ClassName) => collOps.getAcc(newChildrenByParent, name)

    // Walk the tree to add children
    if (batchMode) {
      objectClass.walkForAdditions(getNewChildren)
    } else {
      val existingParents =
        collOps.parFlatMapKeys(newChildrenByParent)(classes.get)
      collOps.foreach(existingParents) { parent =>
        parent.walkForAdditions(getNewChildren)
      }
    }

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
  private abstract class MethodContainer(val className: ClassName,
      val namespace: MemberNamespace) {

    def thisType: Type =
      if (namespace.isStatic) NoType
      else ClassType(className)

    val methods = mutable.Map.empty[MethodName, MethodImpl]

    def optimizedDefs: List[Versioned[MethodDef]] = {
      (for {
        method <- methods.values
        if !method.deleted
      } yield {
        method.optimizedMethodDef
      }).toList
    }

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
        _.value.flags.namespace.ordinal == applicableNamespaceOrdinal
      }

      val newMethodNames = linkedMethodDefs.map(_.value.methodName).toSet
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

      this match {
        case cls: Class =>
          cls.fields = linkedClass.fields
        case _          =>
      }

      for (linkedMethodDef <- linkedMethodDefs) {
        val methodName = linkedMethodDef.value.methodName

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
  private final class Class(val superClass: Option[Class], _className: ClassName)
      extends MethodContainer(_className, MemberNamespace.Public) {

    val myInterface = getInterface(_className)

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

    var interfaces: Set[InterfaceType] = Set.empty
    var subclasses: collOps.ParIterable[Class] = collOps.emptyParIterable
    var isInstantiated: Boolean = false

    var hasElidableModuleAccessor: Boolean = false

    var fields: List[AnyFieldDef] = Nil
    var tryNewInlineable: Option[OptimizerCore.InlineableClassStructure] = None

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
      classes -= className
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

      // Module class specifics
      updateHasElidableModuleAccessor(linkedClass)

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

    /** UPDATE PASS ONLY. */
    def walkForAdditions(
        getNewChildren: ClassName => collOps.ParIterable[LinkedClass]): Unit = {

      val subclassAcc = collOps.prepAdd(subclasses)

      collOps.foreach(getNewChildren(className)) { linkedClass =>
        val cls = new Class(Some(this), linkedClass.className)
        collOps.add(subclassAcc, cls)
        classes += linkedClass.className -> cls
        cls.setupAfterCreation(linkedClass)
        cls.walkForAdditions(getNewChildren)
      }

      subclasses = collOps.finishAdd(subclassAcc)
    }

    /** UPDATE PASS ONLY. */
    def updateHasElidableModuleAccessor(linkedClass: LinkedClass): Unit = {
      def lookupModuleConstructor: Option[MethodImpl] = {
        getInterface(className)
          .staticLike(MemberNamespace.Constructor)
          .methods
          .get(NoArgConstructorName)
      }

      val isModuleClass = linkedClass.kind == ClassKind.ModuleClass

      hasElidableModuleAccessor =
        isAdHocElidableModuleAccessor(className) ||
        (isModuleClass && lookupModuleConstructor.exists(isElidableModuleConstructor))
    }

    /** UPDATE PASS ONLY. */
    def updateTryNewInlineable(linkedClass: LinkedClass): Boolean = {
      val oldTryNewInlineable = tryNewInlineable

      tryNewInlineable = if (!linkedClass.optimizerHints.inline) {
        None
      } else {
        val allFields = for {
          parent <- reverseParentChain
          field <- parent.fields
          if !field.flags.namespace.isStatic
        } yield {
          parent.className -> field
        }

        if (allFields.forall(_._2.isInstanceOf[FieldDef])) {
          Some(new OptimizerCore.InlineableClassStructure(
              allFields.asInstanceOf[List[(ClassName, FieldDef)]]))
        } else {
          None
        }
      }

      tryNewInlineable != oldTryNewInlineable
    }

    /** UPDATE PASS ONLY. */
    def setupAfterCreation(linkedClass: LinkedClass): Unit = {

      updateWith(linkedClass)
      interfaces = linkedClass.ancestors.map(getInterface).toSet

      isInstantiated = linkedClass.hasInstances

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

      updateHasElidableModuleAccessor(linkedClass)
      updateTryNewInlineable(linkedClass)
    }

    /** UPDATE PASS ONLY. */
    private def isElidableModuleConstructor(impl: MethodImpl): Boolean = {
      def isTriviallySideEffectFree(tree: Tree): Boolean = tree match {
        case _:VarRef | _:Literal | _:This | _:Skip => true
        case _                                      => false
      }
      def isElidableStat(tree: Tree): Boolean = tree match {
        case Block(stats)                      => stats.forall(isElidableStat)
        case Assign(Select(This(), _, _), rhs) => isTriviallySideEffectFree(rhs)

        // Mixin constructor, 2.11
        case ApplyStatic(flags, className, methodName, List(This()))
            if !flags.isPrivate =>
          val container =
            getInterface(className).staticLike(MemberNamespace.PublicStatic)
          container.methods(methodName.name).originalDef.body.exists {
            case Skip() => true
            case _      => false
          }

        // Mixin constructor, 2.12+
        case ApplyStatically(flags, This(), className, methodName, Nil)
            if !flags.isPrivate && !classes.contains(className) =>
          // Since className is not in classes, it must be a default method call.
          val container =
            getInterface(className).staticLike(MemberNamespace.Public)
          container.methods.get(methodName.name) exists { methodDef =>
            methodDef.originalDef.body exists {
              case Skip() => true
              case _      => false
            }
          }

        // Delegation to another constructor (super or in the same class)
        case ApplyStatically(flags, This(), className, methodName, args)
            if flags.isConstructor =>
          args.forall(isTriviallySideEffectFree) && {
            getInterface(className)
              .staticLike(MemberNamespace.Constructor)
              .methods
              .get(methodName.name)
              .exists(isElidableModuleConstructor)
          }

        case StoreModule(_, _) => true
        case _                 => isTriviallySideEffectFree(tree)
      }
      impl.originalDef.body.fold {
        throw new AssertionError("Module constructor cannot be abstract")
      } { body =>
        isElidableStat(body)
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
  }

  /** Namespace for static members of a class. */
  private final class StaticLikeNamespace(className: ClassName,
      namespace: MemberNamespace)
      extends MethodContainer(className, namespace) {

    /** BOTH PASSES. */
    final def lookupMethod(methodName: MethodName): Option[MethodImpl] =
      methods.get(methodName)
  }

  /** Thing from which a [[MethodImpl]] can unregister itself from. */
  private trait Unregisterable {
    /** UPDATE PASS ONLY. */
    def unregisterDependee(dependee: MethodImpl): Unit
  }

  /** Type of a class or interface.
   *
   *  There exists exactly one instance of this per LinkedClass.
   *
   *  Fully concurrency safe unless otherwise noted.
   */
  private final class InterfaceType(
      val className: ClassName) extends Unregisterable {

    private type MethodCallers = collOps.Map[MethodName, collOps.Map[MethodImpl, Unit]]

    private val ancestorsAskers = collOps.emptyMap[MethodImpl, Unit]
    private val dynamicCallers: MethodCallers = collOps.emptyMap

    // ArrayBuffer to avoid need for ClassTag[collOps.Map[_, _]]
    private val staticCallers =
      mutable.ArrayBuffer.fill[MethodCallers](MemberNamespace.Count)(collOps.emptyMap)

    private var _ancestors: List[ClassName] = className :: Nil

    private val _instantiatedSubclasses = collOps.emptyMap[Class, Unit]

    private val staticLikes: Array[StaticLikeNamespace] = {
      Array.tabulate(MemberNamespace.Count) { ord =>
        new StaticLikeNamespace(className, MemberNamespace.fromOrdinal(ord))
      }
    }

    override def toString(): String =
      s"intf ${className.nameString}"

    /** PROCESS PASS ONLY. */
    def askDynamicCallTargets(methodName: MethodName,
        asker: MethodImpl): List[MethodImpl] = {
      dynamicCallers
        .getOrElseUpdate(methodName, collOps.emptyMap)
        .put(asker, ())
      asker.registerTo(this)
      _instantiatedSubclasses.keys.flatMap(_.lookupMethod(methodName)).toList
    }

    /** PROCESS PASS ONLY. */
    def askStaticCallTarget(namespace: MemberNamespace, methodName: MethodName,
        asker: MethodImpl): MethodImpl = {
      staticCallers(namespace.ordinal)
        .getOrElseUpdate(methodName, collOps.emptyMap)
        .put(asker, ())
      asker.registerTo(this)

      def inStaticsLike = staticLike(namespace)

      val container =
        if (namespace != MemberNamespace.Public) inStaticsLike
        else classes.getOrElse(className, inStaticsLike)

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
      _instantiatedSubclasses -= x

    /** PROCESS PASS ONLY. */
    def askAncestors(asker: MethodImpl): List[ClassName] = {
      ancestorsAskers.put(asker, ())
      asker.registerTo(this)
      _ancestors
    }

    @inline
    def staticLike(namespace: MemberNamespace): StaticLikeNamespace =
      staticLikes(namespace.ordinal)

    /** UPDATE PASS ONLY. Not concurrency safe. */
    def updateWith(linkedClass: LinkedClass): Unit = {
      // Update ancestors
      if (linkedClass.ancestors != _ancestors) {
        _ancestors = linkedClass.ancestors
        ancestorsAskers.keysIterator.foreach(_.tag())
        ancestorsAskers.clear()
      }

      // Update static likes
      for (staticLike <- staticLikes) {
        val (_, changed, _) = staticLike.updateWith(linkedClass)
        for (method <- changed) {
          this.tagStaticCallersOf(staticLike.namespace, method)
        }
      }
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
      dynamicCallers.remove(methodName)
        .foreach(_.keysIterator.foreach(_.tag()))
    }

    /** Tag the static-callers of an instance method.
     *  UPDATE PASS ONLY.
     */
    def tagStaticCallersOf(namespace: MemberNamespace,
        methodName: MethodName): Unit = {
      staticCallers(namespace.ordinal).remove(methodName)
        .foreach(_.keysIterator.foreach(_.tag()))
    }

    /** UPDATE PASS ONLY. */
    def unregisterDependee(dependee: MethodImpl): Unit = {
      ancestorsAskers.remove(dependee)
      dynamicCallers.valuesIterator.foreach(_.remove(dependee))
      staticCallers.foreach(_.valuesIterator.foreach(_.remove(dependee)))
    }
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
      extends OptimizerCore.MethodImpl with OptimizerCore.AbstractMethodID
      with Unregisterable {

    private[this] var _deleted: Boolean = false

    private val bodyAskers = collOps.emptyMap[MethodImpl, Unit]
    private val registeredTo = collOps.emptyMap[Unregisterable, Unit]
    private val tagged = new AtomicBoolean(false)

    var lastInVersion: Option[String] = None
    var lastOutVersion: Int = 0

    var optimizerHints: OptimizerHints = OptimizerHints.empty
    var originalDef: MethodDef = _
    var optimizedMethodDef: Versioned[MethodDef] = _

    var attributes: Attributes = _

    def enclosingClassName: ClassName = owner.className

    def thisType: Type = owner.thisType
    def deleted: Boolean = _deleted

    override def toString(): String =
      s"$owner.${methodName.nameString}"

    /** PROCESS PASS ONLY. */
    def askBody(asker: MethodImpl): MethodDef = {
      bodyAskers.put(asker, ())
      asker.registerTo(this)
      originalDef
    }

    /** UPDATE PASS ONLY. */
    def tagBodyAskers(): Unit = {
      bodyAskers.keysIterator.foreach(_.tag())
      bodyAskers.clear()
    }

    /** UPDATE PASS ONLY. */
    def unregisterDependee(dependee: MethodImpl): Unit =
      bodyAskers.remove(dependee)

    def registerTo(unregisterable: Unregisterable): Unit =
      registeredTo.put(unregisterable, ())

    /** UPDATE PASS ONLY. */
    private def unregisterFromEverywhere(): Unit = {
      registeredTo.keysIterator.foreach(_.unregisterDependee(this))
      registeredTo.clear()
    }

    /** Tag this method and return true iff it wasn't tagged before.
     *  UPDATE PASS ONLY.
     */
    private def protectTag(): Boolean = !tagged.getAndSet(true)

    /** Returns true if the method's attributes changed.
     *  Attributes are whether it is inlineable, and whether it is a trait
     *  impl forwarder. Basically this is what is declared in
     *  `OptimizerCore.AbstractMethodID`.
     *  In the process, tags all the body askers if the body changes.
     *  UPDATE PASS ONLY. Not concurrency safe on same instance.
     */
    def updateWith(linkedMethod: Versioned[MethodDef]): Boolean = {
      assert(!_deleted, "updateWith() called on a deleted method")

      if (lastInVersion.isDefined && lastInVersion == linkedMethod.version) {
        false
      } else {
        lastInVersion = linkedMethod.version

        val methodDef = linkedMethod.value

        val changed = {
          originalDef == null ||
          (methodDef.hash zip originalDef.hash).forall {
            case (h1, h2) => !Hashers.hashesEqual(h1, h2)
          }
        }

        if (changed) {
          tagBodyAskers()

          val oldAttributes = attributes

          optimizerHints = methodDef.optimizerHints
          originalDef = methodDef
          optimizedMethodDef = null
          attributes = computeNewAttributes()
          tag()

          attributes != oldAttributes
        } else {
          false
        }
      }
    }

    /** UPDATE PASS ONLY. Not concurrency safe on same instance. */
    def delete(): Unit = {
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
    def tag(): Unit = if (protectTag()) {
      collOps.add(methodsToProcess, this)
      unregisterFromEverywhere()
    }

    /** PROCESS PASS ONLY. */
    def process(): Unit = if (!_deleted) {
      val optimizedDef = new Optimizer().optimize(thisType, originalDef)
      lastOutVersion += 1
      optimizedMethodDef =
        new Versioned(optimizedDef, Some(lastOutVersion.toString))
      tagged.set(false)
    }

    /** All methods are PROCESS PASS ONLY */
    private class Optimizer extends OptimizerCore(config) {
      type MethodID = MethodImpl

      val myself: MethodImpl.this.type = MethodImpl.this

      protected def getMethodBody(method: MethodID): MethodDef =
        method.askBody(myself)

      /** Look up the targets of a dynamic call to an instance method. */
      protected def dynamicCall(intfName: ClassName,
          methodName: MethodName): List[MethodID] = {
        getInterface(intfName).askDynamicCallTargets(methodName, myself)
      }

      /** Look up the target of a static call to an instance method. */
      protected def staticCall(className: ClassName, namespace: MemberNamespace,
          methodName: MethodName): MethodID = {
        getInterface(className).askStaticCallTarget(namespace, methodName, myself)
      }

      protected def getAncestorsOf(intfName: ClassName): List[ClassName] =
        getInterface(intfName).askAncestors(myself)

      protected def hasElidableModuleAccessor(moduleClassName: ClassName): Boolean =
        classes(moduleClassName).hasElidableModuleAccessor

      protected def tryNewInlineableClass(
          className: ClassName): Option[OptimizerCore.InlineableClassStructure] = {
        classes(className).tryNewInlineable
      }
    }
  }

}

object IncOptimizer {
  def apply(config: CommonPhaseConfig): IncOptimizer =
    new IncOptimizer(config, SeqCollOps)

  private val isAdHocElidableModuleAccessor: Set[ClassName] =
    Set(ClassName("scala.Predef$"))
}
