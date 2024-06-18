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

import scala.collection.mutable

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types.Type
import org.scalajs.ir.Version

import org.scalajs.linker.interface.ModuleKind
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID
import org.scalajs.linker.CollectionsCompat.MutableMapCompatOps

import EmitterNames._

private[emitter] final class KnowledgeGuardian(config: Emitter.Config) {
  import KnowledgeGuardian._

  private var specialInfo: SpecialInfo = _
  private val classes = mutable.Map.empty[ClassName, Class]

  /** Returns `true` if *all* caches should be invalidated.
   *
   *  For global properties that are rarely changed and heavily used (such as
   *  isParentDataAccessed), we do not want to pay the price of the
   *  dependency graph, in terms of memory consumption and time spent
   *  maintaining it. It is a better trade-off to invalidate everything in
   *  the rare events where they do change.
   */
  def update(moduleSet: ModuleSet): Boolean = {
    val hasInlineableInit = computeHasInlineableInit(moduleSet)
    val staticFieldMirrors = computeStaticFieldMirrors(moduleSet)

    // Object is optional, because the module splitter might remove everything.
    var objectClass: Option[LinkedClass] = None
    var classClass: Option[LinkedClass] = None
    var arithmeticExceptionClass: Option[LinkedClass] = None
    val hijackedClasses = Iterable.newBuilder[LinkedClass]

    // Update classes
    for {
      module <- moduleSet.modules
      linkedClass <- module.classDefs
    } {
      updateClass(linkedClass, Some(module.id))
    }

    moduleSet.abstractClasses.foreach(updateClass(_, module = None))

    def updateClass(linkedClass: LinkedClass, module: Option[ModuleID]): Unit = {
      val className = linkedClass.className
      val thisClassHasInlineableInit = hasInlineableInit(className)
      val thisClassStaticFieldMirrors =
        staticFieldMirrors.getOrElse(className, Map.empty)

      classes.get(className).fold[Unit] {
        // new class
        classes.put(className,
            new Class(linkedClass, thisClassHasInlineableInit, thisClassStaticFieldMirrors, module))
      } { existingCls =>
        existingCls.update(linkedClass, thisClassHasInlineableInit, thisClassStaticFieldMirrors, module)
      }

      linkedClass.className match {
        case ClassClass =>
          classClass = Some(linkedClass)

        case ObjectClass =>
          objectClass = Some(linkedClass)

        case ArithmeticExceptionClass =>
          arithmeticExceptionClass = Some(linkedClass)

        case name if HijackedClasses(name) =>
          hijackedClasses += linkedClass

        case _ =>
      }
    }

    // Garbage collection
    classes.filterInPlace((_, cls) => cls.testAndResetIsAlive())

    val invalidateAll = {
      if (specialInfo == null) {
        specialInfo = new SpecialInfo(objectClass, classClass,
            arithmeticExceptionClass, hijackedClasses.result())
        false
      } else {
        specialInfo.update(objectClass, classClass, arithmeticExceptionClass,
            hijackedClasses.result())
      }
    }

    if (invalidateAll) {
      classes.valuesIterator.foreach(_.unregisterAll())
      specialInfo.unregisterAll()
    }

    invalidateAll
  }

  private def computeHasInlineableInit(moduleSet: ModuleSet): Set[ClassName] = {
    val scalaClassDefs = moduleSet.modules
      .flatMap(_.classDefs)
      .filter(_.kind.isClass)

    val classesWithInstantiatedSubclasses = scalaClassDefs
      .withFilter(_.hasInstances)
      .flatMap(_.superClass)
      .map(_.name)
      .toSet

    def enableInlineableInitFor(classDef: LinkedClass): Boolean = {
      /* We can enable inlined init if all of the following apply:
       * - It does not have any instantiated subclass
       * - It has exactly one constructor
       *
       * By construction, this is always true for module classes.
       */
      !classesWithInstantiatedSubclasses(classDef.className) && {
        classDef.methods.count(
            x => x.flags.namespace == MemberNamespace.Constructor) == 1
      }
    }

    scalaClassDefs
      .withFilter(enableInlineableInitFor(_))
      .map(_.className)
      .toSet
  }

  private def computeStaticFieldMirrors(
      moduleSet: ModuleSet): Map[ClassName, Map[FieldName, List[String]]] = {
    if (config.coreSpec.moduleKind != ModuleKind.NoModule) {
      Map.empty
    } else {
      var result = Map.empty[ClassName, Map[FieldName, List[String]]]
      for {
        module <- moduleSet.modules
        export <- module.topLevelExports
      } {
        export.tree match {
          case TopLevelFieldExportDef(_, exportName, FieldIdent(fieldName)) =>
            val className = export.owningClass
            val mirrors = result.getOrElse(className, Map.empty)
            val newExportNames = exportName :: mirrors.getOrElse(fieldName, Nil)
            val newMirrors = mirrors.updated(fieldName, newExportNames)
            result = result.updated(className, newMirrors)

          case _ =>
        }
      }
      result
    }
  }

  abstract class KnowledgeAccessor extends GlobalKnowledge with Invalidatable {
    /* In theory, a KnowledgeAccessor should *contain* a GlobalKnowledge, not
     * *be* a GlobalKnowledge. We organize it that way to reduce memory
     * footprint and pointer indirections.
     */

    def isParentDataAccessed: Boolean =
      specialInfo.askIsParentDataAccessed(this)

    def isClassClassInstantiated: Boolean =
      specialInfo.askIsClassClassInstantiated(this)

    def isArithmeticExceptionClassInstantiated: Boolean =
      specialInfo.askIsArithmeticExceptionClassInstantiated(this)

    def isInterface(className: ClassName): Boolean =
      classes(className).askIsInterface(this)

    def getAllScalaClassFieldDefs(className: ClassName): List[AnyFieldDef] =
      classes(className).askAllScalaClassFieldDefs(this)

    def hasInlineableInit(className: ClassName): Boolean =
      classes(className).askHasInlineableInit(this)

    def hasStoredSuperClass(className: ClassName): Boolean =
      classes(className).askHasStoredSuperClass(this)

    def hasInstances(className: ClassName): Boolean =
      classes(className).askHasInstances(this)

    def getJSClassCaptureTypes(className: ClassName): Option[List[Type]] =
      classes(className).askJSClassCaptureTypes(this)

    def getJSNativeLoadSpec(className: ClassName): Option[JSNativeLoadSpec] =
      classes(className).askJSNativeLoadSpec(this)

    def getJSNativeLoadSpec(className: ClassName, member: MethodName): JSNativeLoadSpec =
      classes(className).askJSNativeLoadSpec(this, member)

    def getSuperClassOfJSClass(className: ClassName): ClassName =
      classes(className).askJSSuperClass(this)

    def getFieldDefs(className: ClassName): List[AnyFieldDef] =
      classes(className).askFieldDefs(this)

    def getStaticFieldMirrors(field: FieldName): List[String] =
      classes(field.className).askStaticFieldMirrors(this, field)

    def getModule(className: ClassName): ModuleID =
      classes(className).askModule(this)

    def methodsInRepresentativeClasses(): List[(MethodName, Set[ClassName])] =
      specialInfo.askMethodsInRepresentativeClasses(this)

    def methodsInObject(): List[MethodDef] =
      specialInfo.askMethodsInObject(this)

    def hijackedDescendants(className: ClassName): Set[ClassName] =
      specialInfo.askHijackedDescendants(this).getOrElse(className, Set.empty)

    def isAncestorOfHijackedClass(className: ClassName): Boolean =
      specialInfo.askHijackedDescendants(this).contains(className)
  }

  private class Class(initClass: LinkedClass,
      initHasInlineableInit: Boolean,
      initStaticFieldMirrors: Map[FieldName, List[String]],
      initModule: Option[ModuleID])
      extends Unregisterable {

    private val className = initClass.className

    private var isAlive: Boolean = true

    private var isInterface = computeIsInterface(initClass)
    private var hasInlineableInit = initHasInlineableInit
    private var hasStoredSuperClass = computeHasStoredSuperClass(initClass)
    private var hasInstances = initClass.hasInstances
    private var jsClassCaptureTypes = computeJSClassCaptureTypes(initClass)
    private var jsNativeLoadSpec = computeJSNativeLoadSpec(initClass)
    private var jsNativeMemberLoadSpecs = computeJSNativeMemberLoadSpecs(initClass)
    private var superClass = computeSuperClass(initClass)
    private var fieldDefsVersion = computeFieldDefsVersion(initClass)
    private var fieldDefs = computeFieldDefs(initClass)
    private var staticFieldMirrors = initStaticFieldMirrors
    private var module = initModule

    private val isInterfaceAskers = mutable.Set.empty[Invalidatable]
    private val hasInlineableInitAskers = mutable.Set.empty[Invalidatable]
    private val hasStoredSuperClassAskers = mutable.Set.empty[Invalidatable]
    private val hasInstancesAskers = mutable.Set.empty[Invalidatable]
    private val jsClassCaptureTypesAskers = mutable.Set.empty[Invalidatable]
    private val jsNativeLoadSpecAskers = mutable.Set.empty[Invalidatable]
    private val jsNativeMemberLoadSpecsAskers = mutable.Set.empty[Invalidatable]
    private val superClassAskers = mutable.Set.empty[Invalidatable]
    private val fieldDefsAskers = mutable.Set.empty[Invalidatable]
    private val staticFieldMirrorsAskers = mutable.Set.empty[Invalidatable]
    private val moduleAskers = mutable.Set.empty[Invalidatable]

    def update(linkedClass: LinkedClass, newHasInlineableInit: Boolean,
        newStaticFieldMirrors: Map[FieldName, List[String]],
        newModule: Option[ModuleID]): Unit = {
      isAlive = true

      val newIsInterface = computeIsInterface(linkedClass)
      if (newIsInterface != isInterface) {
        isInterface = newIsInterface
        invalidateAskers(isInterfaceAskers)
      }

      if (newHasInlineableInit != hasInlineableInit) {
        hasInlineableInit = newHasInlineableInit
        invalidateAskers(hasInlineableInitAskers)
      }

      val newHasStoredSuperClass = computeHasStoredSuperClass(linkedClass)
      if (newHasStoredSuperClass != hasStoredSuperClass) {
        hasStoredSuperClass = newHasStoredSuperClass
        invalidateAskers(hasStoredSuperClassAskers)
      }

      val newHasInstances = linkedClass.hasInstances
      if (newHasInstances != hasInstances) {
        hasInstances = newHasInstances
        invalidateAskers(hasInstancesAskers)
      }

      val newJSClassCaptureTypes = computeJSClassCaptureTypes(linkedClass)
      if (newJSClassCaptureTypes != jsClassCaptureTypes) {
        jsClassCaptureTypes = newJSClassCaptureTypes
        invalidateAskers(jsClassCaptureTypesAskers)
      }

      val newJSNativeLoadSpec = computeJSNativeLoadSpec(linkedClass)
      if (newJSNativeLoadSpec != jsNativeLoadSpec) {
        jsNativeLoadSpec = newJSNativeLoadSpec
        invalidateAskers(jsNativeLoadSpecAskers)
      }

      val newJSNativeMemberLoadSpecs = computeJSNativeMemberLoadSpecs(linkedClass)
      if (newJSNativeMemberLoadSpecs != jsNativeMemberLoadSpecs) {
        jsNativeMemberLoadSpecs = newJSNativeMemberLoadSpecs
        invalidateAskers(jsNativeMemberLoadSpecsAskers)
      }

      val newSuperClass = computeSuperClass(linkedClass)
      if (newSuperClass != superClass) {
        superClass = newSuperClass
        invalidateAskers(superClassAskers)
      }

      val newFieldDefsVersion = computeFieldDefsVersion(linkedClass)
      if (!newFieldDefsVersion.sameVersion(fieldDefsVersion)) {
        fieldDefsVersion = newFieldDefsVersion
        fieldDefs = computeFieldDefs(linkedClass)
        invalidateAskers(fieldDefsAskers)
      }

      if (newStaticFieldMirrors != staticFieldMirrors) {
        staticFieldMirrors = newStaticFieldMirrors
        invalidateAskers(staticFieldMirrorsAskers)
      }

      if (newModule != module) {
        module = newModule
        invalidateAskers(moduleAskers)
      }
    }

    private def computeIsInterface(linkedClass: LinkedClass): Boolean =
      linkedClass.kind == ClassKind.Interface

    private def computeHasStoredSuperClass(linkedClass: LinkedClass): Boolean =
      linkedClass.jsSuperClass.isDefined

    private def computeJSClassCaptureTypes(linkedClass: LinkedClass): Option[List[Type]] =
      linkedClass.jsClassCaptures.map(_.map(_.ptpe))

    private def computeJSNativeLoadSpec(linkedClass: LinkedClass): Option[JSNativeLoadSpec] =
      linkedClass.jsNativeLoadSpec

    private def computeJSNativeMemberLoadSpecs(
        linkedClass: LinkedClass): Map[MethodName, JSNativeLoadSpec] = {
      if (linkedClass.jsNativeMembers.isEmpty) {
        // Fast path
        Map.empty
      } else {
        linkedClass.jsNativeMembers
          .map(m => m.name.name -> m.jsNativeLoadSpec)
          .toMap
      }
    }

    private def computeSuperClass(linkedClass: LinkedClass): ClassName =
      linkedClass.superClass.fold[ClassName](null.asInstanceOf[ClassName])(_.name)

    /** Computes the version of the fields of a `LinkedClass`.
     *
     *  The version is composed of
     *
     *  - the `version` of the `LinkedClass` itself, which will change every
     *    time the definition of a field changes,
     *  - a boolean indicating whether there is at least one `JSFieldDef`,
     *    which will change every time the reachability analysis of the
     *    `JSFieldDef`s changes (because we either keep all or none of
     *    them), and
     *  - the list of simple names of the `FieldDef`s, which will change every
     *    time the reachability analysis of the `FieldDef`s changes.
     *
     *  We do not try to use the names of `JSFieldDef`s because they are
     *  `Tree`s, which are not efficiently comparable nor versionable here.
     */
    private def computeFieldDefsVersion(linkedClass: LinkedClass): Version = {
      val hasAnyJSField = linkedClass.fields.exists(_.isInstanceOf[JSFieldDef])
      val hasAnyJSFieldVersion = Version.fromByte(if (hasAnyJSField) 1 else 0)
      val scalaFieldNamesVersion = linkedClass.fields.collect {
        case FieldDef(_, FieldIdent(name), _, _) => Version.fromUTF8String(name.simpleName.encoded)
      }
      Version.combine((linkedClass.version :: hasAnyJSFieldVersion :: scalaFieldNamesVersion): _*)
    }

    private def computeFieldDefs(linkedClass: LinkedClass): List[AnyFieldDef] =
      linkedClass.fields

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

    def askAllScalaClassFieldDefs(invalidatable: Invalidatable): List[AnyFieldDef] = {
      invalidatable.registeredTo(this)
      superClassAskers += invalidatable
      fieldDefsAskers += invalidatable
      val inheritedFieldDefs =
        if (superClass == null) Nil
        else classes(superClass).askAllScalaClassFieldDefs(invalidatable)
      inheritedFieldDefs ::: fieldDefs
    }

    def askHasInlineableInit(invalidatable: Invalidatable): Boolean = {
      invalidatable.registeredTo(this)
      hasInlineableInitAskers += invalidatable
      hasInlineableInit
    }

    def askHasStoredSuperClass(invalidatable: Invalidatable): Boolean = {
      invalidatable.registeredTo(this)
      hasStoredSuperClassAskers += invalidatable
      hasStoredSuperClass
    }

    def askHasInstances(invalidatable: Invalidatable): Boolean = {
      invalidatable.registeredTo(this)
      hasInstancesAskers += invalidatable
      hasInstances
    }

    def askJSClassCaptureTypes(invalidatable: Invalidatable): Option[List[Type]] = {
      invalidatable.registeredTo(this)
      jsClassCaptureTypesAskers += invalidatable
      jsClassCaptureTypes
    }

    def askJSNativeLoadSpec(invalidatable: Invalidatable): Option[JSNativeLoadSpec] = {
      invalidatable.registeredTo(this)
      jsNativeLoadSpecAskers += invalidatable
      jsNativeLoadSpec
    }

    def askJSNativeLoadSpec(invalidatable: Invalidatable, member: MethodName): JSNativeLoadSpec = {
      invalidatable.registeredTo(this)
      jsNativeMemberLoadSpecsAskers += invalidatable
      jsNativeMemberLoadSpecs(member)
    }

    def askJSSuperClass(invalidatable: Invalidatable): ClassName = {
      invalidatable.registeredTo(this)
      superClassAskers += invalidatable
      superClass
    }

    def askFieldDefs(invalidatable: Invalidatable): List[AnyFieldDef] = {
      invalidatable.registeredTo(this)
      fieldDefsAskers += invalidatable
      fieldDefs
    }

    def askStaticFieldMirrors(invalidatable: Invalidatable,
        field: FieldName): List[String] = {
      invalidatable.registeredTo(this)
      staticFieldMirrorsAskers += invalidatable
      staticFieldMirrors.getOrElse(field, Nil)
    }

    def askModule(invalidatable: Invalidatable): ModuleID = {
      invalidatable.registeredTo(this)
      moduleAskers += invalidatable
      module.getOrElse {
        throw new AssertionError(
            "trying to get module of abstract class " + className.nameString)
      }
    }

    def unregister(invalidatable: Invalidatable): Unit = {
      isInterfaceAskers -= invalidatable
      hasInlineableInitAskers -= invalidatable
      hasStoredSuperClassAskers -= invalidatable
      hasInstancesAskers -= invalidatable
      jsClassCaptureTypesAskers -= invalidatable
      jsNativeLoadSpecAskers -= invalidatable
      jsNativeMemberLoadSpecsAskers -= invalidatable
      superClassAskers -= invalidatable
      fieldDefsAskers -= invalidatable
      staticFieldMirrorsAskers -= invalidatable
      moduleAskers -= invalidatable
    }

    /** Call this when we invalidate all caches. */
    def unregisterAll(): Unit = {
      isInterfaceAskers.clear()
      hasInlineableInitAskers.clear()
      hasStoredSuperClassAskers.clear()
      hasInstancesAskers.clear()
      jsClassCaptureTypesAskers.clear()
      jsNativeLoadSpecAskers.clear()
      jsNativeMemberLoadSpecsAskers.clear()
      superClassAskers.clear()
      fieldDefsAskers.clear()
      staticFieldMirrorsAskers.clear()
      moduleAskers.clear()
    }
  }

  private class SpecialInfo(initObjectClass: Option[LinkedClass],
      initClassClass: Option[LinkedClass],
      initArithmeticExceptionClass: Option[LinkedClass],
      initHijackedClasses: Iterable[LinkedClass]) extends Unregisterable {

    import SpecialInfo._

    private var instantiatedSpecialClassBitSet =
      computeInstantiatedSpecialClassBitSet(initClassClass, initArithmeticExceptionClass)

    private var isParentDataAccessed =
      computeIsParentDataAccessed(initClassClass)

    private var methodsInRepresentativeClasses =
      computeMethodsInRepresentativeClasses(initObjectClass, initHijackedClasses)

    private var methodsInObject =
      computeMethodsInObject(initObjectClass)

    private var hijackedDescendants =
      computeHijackedDescendants(initHijackedClasses)

    // Askers of isXClassInstantiated -- merged for all X because in practice that's only the CoreJSLib
    private val instantiatedSpecialClassAskers = mutable.Set.empty[Invalidatable]

    private val methodsInRepresentativeClassesAskers = mutable.Set.empty[Invalidatable]
    private val methodsInObjectAskers = mutable.Set.empty[Invalidatable]

    def update(objectClass: Option[LinkedClass], classClass: Option[LinkedClass],
        arithmeticExceptionClass: Option[LinkedClass],
        hijackedClasses: Iterable[LinkedClass]): Boolean = {
      var invalidateAll = false

      val newInstantiatedSpecialClassBitSet =
        computeInstantiatedSpecialClassBitSet(classClass, arithmeticExceptionClass)
      if (newInstantiatedSpecialClassBitSet != instantiatedSpecialClassBitSet) {
        instantiatedSpecialClassBitSet = newInstantiatedSpecialClassBitSet
        invalidateAskers(instantiatedSpecialClassAskers)
      }

      val newIsParentDataAccessed = computeIsParentDataAccessed(classClass)
      if (newIsParentDataAccessed != isParentDataAccessed) {
        isParentDataAccessed = newIsParentDataAccessed
        invalidateAll = true
      }

      val newMethodsInRepresentativeClasses =
          computeMethodsInRepresentativeClasses(objectClass, hijackedClasses)
      if (newMethodsInRepresentativeClasses != methodsInRepresentativeClasses) {
        methodsInRepresentativeClasses = newMethodsInRepresentativeClasses
        invalidateAskers(methodsInRepresentativeClassesAskers)
      }

      /* Usage-sites of methodsInObject never cache.
       * Therefore, we do not bother comparing (which is expensive), but simply
       * invalidate.
       */
      methodsInObject = computeMethodsInObject(objectClass)
      invalidateAskers(methodsInObjectAskers)

      val newHijackedDescendants = computeHijackedDescendants(hijackedClasses)
      if (newHijackedDescendants != hijackedDescendants) {
        hijackedDescendants = newHijackedDescendants
        invalidateAll = true
      }

      invalidateAll
    }

    private def computeInstantiatedSpecialClassBitSet(
        classClass: Option[LinkedClass],
        arithmeticExceptionClass: Option[LinkedClass]): Int = {
      var bitSet: Int = 0
      if (classClass.exists(_.hasInstances))
        bitSet |= SpecialClassClass
      if (arithmeticExceptionClass.exists(_.hasInstances))
        bitSet |= SpecialClassArithmeticException
      bitSet
    }

    private def computeIsParentDataAccessed(classClass: Option[LinkedClass]): Boolean = {
      def methodExists(linkedClass: LinkedClass, methodName: MethodName): Boolean = {
        linkedClass.methods.exists { m =>
          m.flags.namespace == MemberNamespace.Public &&
          m.methodName == methodName
        }
      }

      classClass.exists(methodExists(_, getSuperclassMethodName))
    }

    private def computeMethodsInRepresentativeClasses(objectClass: Option[LinkedClass],
        hijackedClasses: Iterable[LinkedClass]): List[(MethodName, Set[ClassName])] = {
      val representativeClasses =
        objectClass.iterator ++ hijackedClasses.iterator

      val result = mutable.HashMap.empty[MethodName, mutable.Set[ClassName]]

      for {
        representativeClass <- representativeClasses
        method <- representativeClass.methods
        if method.flags.namespace == MemberNamespace.Public
      } {
        result.getOrElseUpdate(method.methodName, mutable.Set.empty) +=
          representativeClass.className
      }

      result.toList.sortBy(_._1.nameString).map(kv => (kv._1, kv._2.toSet))
    }

    private def computeMethodsInObject(objectClass: Option[LinkedClass]): List[MethodDef] = {
      objectClass.toList.flatMap(
          _.methods.filter(_.flags.namespace == MemberNamespace.Public))
    }

    private def computeHijackedDescendants(
        hijackedClasses: Iterable[LinkedClass]): Map[ClassName, Set[ClassName]] = {
      val pairs = for {
        hijackedClass <- hijackedClasses
        ancestor <- hijackedClass.ancestors
        if ancestor != hijackedClass.className
      } yield {
        (ancestor, hijackedClass)
      }

      for {
        (ancestor, pairs) <- pairs.groupBy(_._1)
      } yield {
        (ancestor, pairs.map(_._2.className).toSet)
      }
    }

    def askIsClassClassInstantiated(invalidatable: Invalidatable): Boolean = {
      invalidatable.registeredTo(this)
      instantiatedSpecialClassAskers += invalidatable
      (instantiatedSpecialClassBitSet & SpecialClassClass) != 0
    }

    def askIsArithmeticExceptionClassInstantiated(invalidatable: Invalidatable): Boolean = {
      invalidatable.registeredTo(this)
      instantiatedSpecialClassAskers += invalidatable
      (instantiatedSpecialClassBitSet & SpecialClassArithmeticException) != 0
    }

    def askIsParentDataAccessed(invalidatable: Invalidatable): Boolean =
      isParentDataAccessed

    def askMethodsInRepresentativeClasses(
        invalidatable: Invalidatable): List[(MethodName, Set[ClassName])] = {
      invalidatable.registeredTo(this)
      methodsInRepresentativeClassesAskers += invalidatable
      methodsInRepresentativeClasses
    }

    def askMethodsInObject(invalidatable: Invalidatable): List[MethodDef] = {
      invalidatable.registeredTo(this)
      methodsInObjectAskers += invalidatable
      methodsInObject
    }

    def askHijackedDescendants(
        invalidatable: Invalidatable): Map[ClassName, Set[ClassName]] = {
      hijackedDescendants
    }

    def unregister(invalidatable: Invalidatable): Unit = {
      instantiatedSpecialClassAskers -= invalidatable
      methodsInRepresentativeClassesAskers -= invalidatable
      methodsInObjectAskers -= invalidatable
    }

    /** Call this when we invalidate all caches. */
    def unregisterAll(): Unit = {
      instantiatedSpecialClassAskers.clear()
      methodsInRepresentativeClassesAskers.clear()
      methodsInObjectAskers.clear()
    }
  }

  private object SpecialInfo {
    private final val SpecialClassClass = 1 << 0
    private final val SpecialClassArithmeticException = 1 << 1
  }

  private def invalidateAskers(askers: mutable.Set[Invalidatable]): Unit = {
    /* Calling `invalidate` cause the `Invalidatable` to call `unregister()` in
     * this class, which will mutate the `askers` set. Therefore, we cannot
     * directly iterate over `askers`, and need to take a snapshot instead.
     */
    val snapshot = askers.toSeq
    askers.clear()
    snapshot.foreach(_.invalidate())
  }
}

private[emitter] object KnowledgeGuardian {
  private trait Unregisterable {
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
}
