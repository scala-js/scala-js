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
import org.scalajs.linker.caching
import org.scalajs.linker.caching._

import EmitterNames._

private[emitter] final class KnowledgeGuardian(config: Emitter.Config) {
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
    var illegalArgumentExceptionClass: Option[LinkedClass] = None
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

        case IllegalArgumentExceptionClass =>
          illegalArgumentExceptionClass = Some(linkedClass)

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
            arithmeticExceptionClass, illegalArgumentExceptionClass,
            hijackedClasses.result(), moduleSet.globalInfo)
        false
      } else {
        specialInfo.update(objectClass, classClass,
            arithmeticExceptionClass, illegalArgumentExceptionClass,
            hijackedClasses.result(), moduleSet.globalInfo)
      }
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

  abstract class KnowledgeAccessor
      extends Cache with GlobalKnowledge with caching.KnowledgeAccessor {

    /* In theory, a KnowledgeAccessor should *contain* a GlobalKnowledge, not
     * *be* a GlobalKnowledge. We organize it that way to reduce memory
     * footprint and pointer indirections.
     */

    def isParentDataAccessed: Boolean =
      specialInfo.askIsParentDataAccessed(this)

    def isClassClassInstantiated: Boolean =
      specialInfo.askIsClassClassInstantiated(this)

    def isArithmeticExceptionClassInstantiatedWithStringArg: Boolean =
      specialInfo.askIsArithmeticExceptionClassInstantiatedWithStringArg(this)

    def isIllegalArgumentExceptionClassInstantiatedWithNoArg: Boolean =
      specialInfo.askIsIllegalArgumentExceptionClassInstantiatedWithNoArg(this)

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
      initModule: Option[ModuleID]) {

    private val className = initClass.className

    private var isAlive: Boolean = true

    private val isInterface = KnowledgeSource(initClass)(computeIsInterface(_))
    private val hasInlineableInit = KnowledgeSource(initHasInlineableInit)(identity)
    private val hasStoredSuperClass = KnowledgeSource(initClass)(computeHasStoredSuperClass(_))
    private val hasInstances = KnowledgeSource(initClass)(_.hasInstances)
    private val jsClassCaptureTypes = KnowledgeSource(initClass)(computeJSClassCaptureTypes(_))
    private val jsNativeLoadSpec = KnowledgeSource(initClass)(computeJSNativeLoadSpec(_))
    private val jsNativeMemberLoadSpecs = KnowledgeSource(initClass)(computeJSNativeMemberLoadSpecs(_))
    private val superClass = KnowledgeSource(initClass)(computeSuperClass(_))

    private val fieldDefs = {
      KnowledgeSource.withCustomComparison(initClass)(computeFieldDefsWithVersion(_))(
          (a, b) => a._2.sameVersion(b._2))
    }

    private val staticFieldMirrors = KnowledgeSource(initStaticFieldMirrors)(identity)
    private val module = KnowledgeSource(initModule)(identity)

    def update(linkedClass: LinkedClass, newHasInlineableInit: Boolean,
        newStaticFieldMirrors: Map[FieldName, List[String]],
        newModule: Option[ModuleID]): Unit = {
      isAlive = true

      isInterface.update(linkedClass)
      hasInlineableInit.update(newHasInlineableInit)
      hasStoredSuperClass.update(linkedClass)
      hasInstances.update(linkedClass)
      jsClassCaptureTypes.update(linkedClass)
      jsNativeLoadSpec.update(linkedClass)
      jsNativeMemberLoadSpecs.update(linkedClass)
      superClass.update(linkedClass)
      fieldDefs.update(linkedClass)
      staticFieldMirrors.update(newStaticFieldMirrors)
      module.update(newModule)
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

    /** Computes the fields of a `LinkedClass` along with a `Version` for them.
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
    private def computeFieldDefsWithVersion(linkedClass: LinkedClass): (List[AnyFieldDef], Version) = {
      val fields = linkedClass.fields
      val hasAnyJSField = fields.exists(_.isInstanceOf[JSFieldDef])
      val hasAnyJSFieldVersion = Version.fromByte(if (hasAnyJSField) 1 else 0)
      val scalaFieldNamesVersion = fields.collect {
        case FieldDef(_, FieldIdent(name), _, _) => Version.fromUTF8String(name.simpleName.encoded)
      }
      val version =
        Version.combine((linkedClass.version :: hasAnyJSFieldVersion :: scalaFieldNamesVersion): _*)
      (fields, version)
    }

    def testAndResetIsAlive(): Boolean = {
      val result = isAlive
      isAlive = false
      result
    }

    def askIsInterface(accessor: KnowledgeAccessor): Boolean =
      isInterface.askKnowledge(accessor)

    def askAllScalaClassFieldDefs(accessor: KnowledgeAccessor): List[AnyFieldDef] = {
      val inheritedFieldDefs = superClass.askKnowledge(accessor) match {
        case null       => Nil
        case superClass => classes(superClass).askAllScalaClassFieldDefs(accessor)
      }
      val myFieldDefs = fieldDefs.askKnowledge(accessor)._1
      inheritedFieldDefs ::: myFieldDefs
    }

    def askHasInlineableInit(accessor: KnowledgeAccessor): Boolean =
      hasInlineableInit.askKnowledge(accessor)

    def askHasStoredSuperClass(accessor: KnowledgeAccessor): Boolean =
      hasStoredSuperClass.askKnowledge(accessor)

    def askHasInstances(accessor: KnowledgeAccessor): Boolean =
      hasInstances.askKnowledge(accessor)

    def askJSClassCaptureTypes(accessor: KnowledgeAccessor): Option[List[Type]] =
      jsClassCaptureTypes.askKnowledge(accessor)

    def askJSNativeLoadSpec(accessor: KnowledgeAccessor): Option[JSNativeLoadSpec] =
      jsNativeLoadSpec.askKnowledge(accessor)

    def askJSNativeLoadSpec(accessor: KnowledgeAccessor, member: MethodName): JSNativeLoadSpec =
      jsNativeMemberLoadSpecs.askKnowledge(accessor)(member)

    def askJSSuperClass(accessor: KnowledgeAccessor): ClassName =
      superClass.askKnowledge(accessor)

    def askFieldDefs(accessor: KnowledgeAccessor): List[AnyFieldDef] =
      fieldDefs.askKnowledge(accessor)._1

    def askStaticFieldMirrors(accessor: KnowledgeAccessor,
        field: FieldName): List[String] = {
      staticFieldMirrors.askKnowledge(accessor).getOrElse(field, Nil)
    }

    def askModule(accessor: KnowledgeAccessor): ModuleID = {
      module.askKnowledge(accessor).getOrElse {
        throw new AssertionError(
            "trying to get module of abstract class " + className.nameString)
      }
    }
  }

  private class SpecialInfo(initObjectClass: Option[LinkedClass],
      initClassClass: Option[LinkedClass],
      initArithmeticExceptionClass: Option[LinkedClass],
      initIllegalArgumentExceptionClass: Option[LinkedClass],
      initHijackedClasses: Iterable[LinkedClass],
      initGlobalInfo: LinkedGlobalInfo) {

    import SpecialInfo._

    /* Knowledge for isXClassInstantiated -- merged for all X because in
     * practice that knowledge is only used by the CoreJSLib.
     */
    private val instantiatedSpecialClassBitSet = {
      KnowledgeSource(initClassClass, initArithmeticExceptionClass,
          initIllegalArgumentExceptionClass)(
          computeInstantiatedSpecialClassBitSet(_, _, _))
    }

    private var isParentDataAccessed =
      computeIsParentDataAccessed(initGlobalInfo)

    private val methodsInRepresentativeClasses = {
      KnowledgeSource(initObjectClass, initHijackedClasses)(
          computeMethodsInRepresentativeClasses(_, _))
    }

    private val methodsInObject = {
      /* Usage-sites of methodsInObject never cache.
       * Since the comparison is expensive, we do not bother.
       * Instead, we always invalidate.
       */
      KnowledgeSource.withCustomComparison(initObjectClass)(
          computeMethodsInObject(_))(
          (a, b) => false)
    }

    private var hijackedDescendants =
      computeHijackedDescendants(initHijackedClasses)

    def update(objectClass: Option[LinkedClass], classClass: Option[LinkedClass],
        arithmeticExceptionClass: Option[LinkedClass],
        illegalArgumentExceptionClass: Option[LinkedClass],
        hijackedClasses: Iterable[LinkedClass],
        globalInfo: LinkedGlobalInfo): Boolean = {
      var invalidateAll = false

      instantiatedSpecialClassBitSet.update(
          (classClass, arithmeticExceptionClass, illegalArgumentExceptionClass))

      val newIsParentDataAccessed = computeIsParentDataAccessed(globalInfo)
      if (newIsParentDataAccessed != isParentDataAccessed) {
        isParentDataAccessed = newIsParentDataAccessed
        invalidateAll = true
      }

      methodsInRepresentativeClasses.update((objectClass, hijackedClasses))
      methodsInObject.update(objectClass)

      val newHijackedDescendants = computeHijackedDescendants(hijackedClasses)
      if (newHijackedDescendants != hijackedDescendants) {
        hijackedDescendants = newHijackedDescendants
        invalidateAll = true
      }

      invalidateAll
    }

    private def computeInstantiatedSpecialClassBitSet(
        classClass: Option[LinkedClass],
        arithmeticExceptionClass: Option[LinkedClass],
        illegalArgumentExceptionClass: Option[LinkedClass]): Int = {

      def isInstantiatedWithCtor(linkedClass: Option[LinkedClass], ctor: MethodName): Boolean = {
        linkedClass.exists { cls =>
          cls.hasDirectInstances && cls.methods.exists(_.methodName == ctor)
        }
      }

      var bitSet: Int = 0
      if (classClass.exists(_.hasDirectInstances))
        bitSet |= SpecialClassClass
      if (isInstantiatedWithCtor(arithmeticExceptionClass, StringArgConstructorName))
        bitSet |= SpecialClassArithmeticExceptionWithStringArg
      if (isInstantiatedWithCtor(illegalArgumentExceptionClass, NoArgConstructorName))
        bitSet |= SpecialClassIllegalArgumentExceptionWithNoArg
      bitSet
    }

    private def computeIsParentDataAccessed(globalInfo: LinkedGlobalInfo): Boolean =
      globalInfo.isClassSuperClassUsed

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

    def askIsClassClassInstantiated(accessor: KnowledgeAccessor): Boolean = {
      val bitSet = instantiatedSpecialClassBitSet.askKnowledge(accessor)
      (bitSet & SpecialClassClass) != 0
    }

    def askIsArithmeticExceptionClassInstantiatedWithStringArg(accessor: KnowledgeAccessor): Boolean = {
      val bitSet = instantiatedSpecialClassBitSet.askKnowledge(accessor)
      (bitSet & SpecialClassArithmeticExceptionWithStringArg) != 0
    }

    def askIsIllegalArgumentExceptionClassInstantiatedWithNoArg(accessor: KnowledgeAccessor): Boolean = {
      val bitSet = instantiatedSpecialClassBitSet.askKnowledge(accessor)
      (bitSet & SpecialClassIllegalArgumentExceptionWithNoArg) != 0
    }

    def askIsParentDataAccessed(accessor: KnowledgeAccessor): Boolean =
      isParentDataAccessed

    def askMethodsInRepresentativeClasses(
        accessor: KnowledgeAccessor): List[(MethodName, Set[ClassName])] = {
      methodsInRepresentativeClasses.askKnowledge(accessor)
    }

    def askMethodsInObject(accessor: KnowledgeAccessor): List[MethodDef] =
      methodsInObject.askKnowledge(accessor)

    def askHijackedDescendants(
        accessor: KnowledgeAccessor): Map[ClassName, Set[ClassName]] = {
      hijackedDescendants
    }
  }

  private object SpecialInfo {
    private final val SpecialClassClass = 1 << 0
    private final val SpecialClassArithmeticExceptionWithStringArg = 1 << 1
    private final val SpecialClassIllegalArgumentExceptionWithNoArg = 1 << 2
  }
}
