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

import org.scalajs.linker.interface.ModuleKind
import org.scalajs.linker.standard._
import org.scalajs.linker.CollectionsCompat.MutableMapCompatOps

import EmitterNames._

private[emitter] final class KnowledgeGuardian(config: CommonPhaseConfig) {
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
  def update(linkingUnit: LinkingUnit): Boolean = {
    val hasInlineableInit = computeHasInlineableInit(linkingUnit)

    var classClass: Option[LinkedClass] = None
    val representativeClasses = Iterable.newBuilder[LinkedClass]

    // Update classes
    for (linkedClass <- linkingUnit.classDefs) {
      val className = linkedClass.className
      val thisClassHasInlineableInit = hasInlineableInit(className)
      classes.get(className).fold[Unit] {
        // new class
        classes.put(className,
            new Class(linkedClass, thisClassHasInlineableInit))
      } { existingCls =>
        existingCls.update(linkedClass, thisClassHasInlineableInit)
      }

      linkedClass.className match {
        case ClassClass =>
          classClass = Some(linkedClass)

        case ObjectClass =>
          representativeClasses += linkedClass

        case name if HijackedClasses(name) =>
          representativeClasses += linkedClass

        case _ =>
      }
    }

    // Garbage collection
    classes.filterInPlace((_, cls) => cls.testAndResetIsAlive())

    val invalidateAll = {
      if (specialInfo == null) {
        specialInfo = new SpecialInfo(classClass, representativeClasses.result())
        false
      } else {
        specialInfo.update(classClass, representativeClasses.result())
      }
    }

    if (invalidateAll) {
      classes.valuesIterator.foreach(_.unregisterAll())
      specialInfo.unregisterAll()
    }

    invalidateAll
  }

  private def computeHasInlineableInit(linkingUnit: LinkingUnit): Set[ClassName] = {
    val scalaClassDefs = linkingUnit.classDefs.filter(_.kind.isClass)

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
            x => x.value.flags.namespace == MemberNamespace.Constructor) == 1
      }
    }

    scalaClassDefs
      .withFilter(enableInlineableInitFor(_))
      .map(_.className)
      .toSet
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

    def isInterface(className: ClassName): Boolean =
      classes(className).askIsInterface(this)

    def getAllScalaClassFieldDefs(className: ClassName): List[(ClassName, List[AnyFieldDef])] =
      classes(className).askAllScalaClassFieldDefs(this)

    def hasInlineableInit(className: ClassName): Boolean =
      classes(className).askHasInlineableInit(this)

    def hasStoredSuperClass(className: ClassName): Boolean =
      classes(className).askHasStoredSuperClass(this)

    def getJSClassCaptureTypes(className: ClassName): Option[List[Type]] =
      classes(className).askJSClassCaptureTypes(this)

    def getJSNativeLoadSpec(className: ClassName): Option[JSNativeLoadSpec] =
      classes(className).askJSNativeLoadSpec(this)

    def getJSNativeLoadSpec(className: ClassName, member: MethodName): JSNativeLoadSpec =
      classes(className).askJSNativeLoadSpec(this, member)

    def getSuperClassOfJSClass(className: ClassName): ClassName =
      classes(className).askJSSuperClass(this)

    def getJSClassFieldDefs(className: ClassName): List[AnyFieldDef] =
      classes(className).askJSClassFieldDefs(this)

    def getStaticFieldMirrors(className: ClassName, field: FieldName): List[String] =
      classes(className).askStaticFieldMirrors(this, field)

    def representativeClassHasPublicMethod(className: ClassName,
        methodName: MethodName): Boolean = {
      specialInfo.askRepresentativeClassHasPublicMethod(this, className, methodName)
    }
  }

  private class Class(initClass: LinkedClass,
      initHasInlineableInit: Boolean)
      extends Unregisterable {

    private val className = initClass.className

    private var isAlive: Boolean = true

    private var isInterface = computeIsInterface(initClass)
    private var hasInlineableInit = initHasInlineableInit
    private var hasStoredSuperClass = computeHasStoredSuperClass(initClass)
    private var jsClassCaptureTypes = computeJSClassCaptureTypes(initClass)
    private var jsNativeLoadSpec = computeJSNativeLoadSpec(initClass)
    private var jsNativeMemberLoadSpecs = computeJSNativeMemberLoadSpecs(initClass)
    private var superClass = computeSuperClass(initClass)
    private var fieldDefs = computeFieldDefs(initClass)
    private var staticFieldMirrors = computeStaticFieldMirrors(initClass)

    private val isInterfaceAskers = mutable.Set.empty[Invalidatable]
    private val hasInlineableInitAskers = mutable.Set.empty[Invalidatable]
    private val hasStoredSuperClassAskers = mutable.Set.empty[Invalidatable]
    private val jsClassCaptureTypesAskers = mutable.Set.empty[Invalidatable]
    private val jsNativeLoadSpecAskers = mutable.Set.empty[Invalidatable]
    private val jsNativeMemberLoadSpecsAskers = mutable.Set.empty[Invalidatable]
    private val superClassAskers = mutable.Set.empty[Invalidatable]
    private val fieldDefsAskers = mutable.Set.empty[Invalidatable]
    private val staticFieldMirrorsAskers = mutable.Set.empty[Invalidatable]

    def update(linkedClass: LinkedClass, newHasInlineableInit: Boolean): Unit = {
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

      val newFieldDefs = computeFieldDefs(linkedClass)
      if (newFieldDefs != fieldDefs) {
        fieldDefs = newFieldDefs
        invalidateAskers(fieldDefsAskers)
      }

      val newStaticFieldMirrors = computeStaticFieldMirrors(linkedClass)
      if (newStaticFieldMirrors != staticFieldMirrors) {
        staticFieldMirrors = newStaticFieldMirrors
        invalidateAskers(staticFieldMirrorsAskers)
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

    private def computeFieldDefs(linkedClass: LinkedClass): List[AnyFieldDef] =
      linkedClass.fields

    private def computeStaticFieldMirrors(
        linkedClass: LinkedClass): Map[FieldName, List[String]] = {
      if (config.coreSpec.moduleKind != ModuleKind.NoModule ||
          linkedClass.topLevelExports.isEmpty) {
        // Fast path
        Map.empty
      } else {
        val result = mutable.Map.empty[FieldName, List[String]]
        for (export <- linkedClass.topLevelExports) {
          export match {
            case TopLevelFieldExportDef(exportName, FieldIdent(fieldName)) =>
              result(fieldName) = exportName :: result.getOrElse(fieldName, Nil)
            case _ =>
              ()
          }
        }
        result.toMap
      }
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

    def askAllScalaClassFieldDefs(
        invalidatable: Invalidatable): List[(ClassName, List[AnyFieldDef])] = {
      invalidatable.registeredTo(this)
      superClassAskers += invalidatable
      fieldDefsAskers += invalidatable
      val inheritedFieldDefs =
        if (superClass == null) Nil
        else classes(superClass).askAllScalaClassFieldDefs(invalidatable)
      inheritedFieldDefs :+ (className -> fieldDefs)
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

    def askJSClassFieldDefs(invalidatable: Invalidatable): List[AnyFieldDef] = {
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

    def unregister(invalidatable: Invalidatable): Unit = {
      isInterfaceAskers -= invalidatable
      hasInlineableInitAskers -= invalidatable
      hasStoredSuperClassAskers -= invalidatable
      jsClassCaptureTypesAskers -= invalidatable
      jsNativeLoadSpecAskers -= invalidatable
      jsNativeMemberLoadSpecsAskers -= invalidatable
      superClassAskers -= invalidatable
      fieldDefsAskers -= invalidatable
      staticFieldMirrorsAskers -= invalidatable
    }

    /** Call this when we invalidate all caches. */
    def unregisterAll(): Unit = {
      isInterfaceAskers.clear()
      hasInlineableInitAskers.clear()
      hasStoredSuperClassAskers.clear()
      jsClassCaptureTypesAskers.clear()
      jsNativeLoadSpecAskers.clear()
      jsNativeMemberLoadSpecsAskers.clear()
      superClassAskers.clear()
      fieldDefsAskers.clear()
      staticFieldMirrorsAskers.clear()
    }
  }

  private class SpecialInfo(initClassClass: Option[LinkedClass],
      initRepresentativeClasses: Iterable[LinkedClass]) extends Unregisterable {

    private var isClassClassInstantiated =
      computeIsClassClassInstantiated(initClassClass)

    private val isClassClassInstantiatedAskers = mutable.Set.empty[Invalidatable]

    private var isParentDataAccessed =
      computeIsParentDataAccessed(initClassClass)

    private var methodsInRepresentativeClasses =
      computeMethodsInRepresentativeClasses(initRepresentativeClasses)

    private val methodsInRepresentativeClassesAskers = mutable.Set.empty[Invalidatable]

    def update(classClass: Option[LinkedClass],
        representativeClasses: Iterable[LinkedClass]): Boolean = {
      val newMethodsInRepresentativeClasses = computeMethodsInRepresentativeClasses(representativeClasses)

      if (newMethodsInRepresentativeClasses != methodsInRepresentativeClasses) {
        methodsInRepresentativeClasses = newMethodsInRepresentativeClasses
        invalidateAskers(methodsInRepresentativeClassesAskers)
      }

      val newIsClassClassInstantiated = computeIsClassClassInstantiated(classClass)
      if (newIsClassClassInstantiated != isClassClassInstantiated) {
        isClassClassInstantiated = newIsClassClassInstantiated
        invalidateAskers(isClassClassInstantiatedAskers)
      }

      val newIsParentDataAccessed = computeIsParentDataAccessed(classClass)

      val invalidateAll = isParentDataAccessed != newIsParentDataAccessed

      invalidateAll
    }

    private def computeIsClassClassInstantiated(classClass: Option[LinkedClass]): Boolean = {
      classClass.exists(_.methods.exists { m =>
        m.value.flags.namespace == MemberNamespace.Constructor
      })
    }

    private def computeIsParentDataAccessed(classClass: Option[LinkedClass]): Boolean = {
      def methodExists(linkedClass: LinkedClass, methodName: MethodName): Boolean = {
        linkedClass.methods.exists { m =>
          m.value.flags.namespace == MemberNamespace.Public &&
          m.value.methodName == methodName
        }
      }

      classClass.exists(methodExists(_, getSuperclassMethodName))
    }

    private def computeMethodsInRepresentativeClasses(
        representativeClasses: Iterable[LinkedClass]): Set[(ClassName, MethodName)] = {
      val pairs = for {
        representativeClass <- representativeClasses
        method <- representativeClass.methods
        if method.value.flags.namespace == MemberNamespace.Public
      } yield {
        (representativeClass.className, method.value.methodName)
      }

      pairs.toSet
    }

    def askIsClassClassInstantiated(invalidatable: Invalidatable): Boolean = {
      invalidatable.registeredTo(this)
      isClassClassInstantiatedAskers += invalidatable
      isClassClassInstantiated
    }

    def askIsParentDataAccessed(invalidatable: Invalidatable): Boolean =
      isParentDataAccessed

    def askRepresentativeClassHasPublicMethod(invalidatable: Invalidatable,
        className: ClassName, methodName: MethodName): Boolean = {
      invalidatable.registeredTo(this)
      methodsInRepresentativeClassesAskers += invalidatable
      methodsInRepresentativeClasses.contains((className, methodName))
    }

    def unregister(invalidatable: Invalidatable): Unit = {
      isClassClassInstantiatedAskers -= invalidatable
      methodsInRepresentativeClassesAskers -= invalidatable
    }

    /** Call this when we invalidate all caches. */
    def unregisterAll(): Unit = {
      isClassClassInstantiatedAskers.clear()
      methodsInRepresentativeClassesAskers.clear()
    }
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
