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

package org.scalajs.linker.analyzer

import scala.annotation.tailrec

import scala.collection.mutable
import scala.concurrent._

import scala.util.{Try, Success, Failure}

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic._

import org.scalajs.ir
import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees.{MemberNamespace, JSNativeLoadSpec}
import org.scalajs.ir.Types.ClassRef

import org.scalajs.linker._
import org.scalajs.linker.frontend.IRLoader
import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID

import org.scalajs.logging._

import Platform._

import Analysis._
import Infos.{NamespacedMethodName, ReachabilityInfo, ReachabilityInfoInClass}

final class Analyzer(config: CommonPhaseConfig, initial: Boolean,
    checkIR: Boolean, failOnError: Boolean, irLoader: IRLoader) {

  private val infoLoader: InfoLoader = {
    new InfoLoader(irLoader,
        if (!checkIR) InfoLoader.NoIRCheck
        else if (initial) InfoLoader.InitialIRCheck
        else InfoLoader.InternalIRCheck,
        config.coreSpec.linkTimeProperties
    )
  }

  def computeReachability(moduleInitializers: Seq[ModuleInitializer],
      symbolRequirements: SymbolRequirement, logger: Logger)(implicit ec: ExecutionContext): Future[Analysis] = {

    infoLoader.update(logger)

    val run = new AnalyzerRun(config, initial, infoLoader)(
        adjustExecutionContextForParallelism(ec, config.parallel))

    run
      .computeReachability(moduleInitializers, symbolRequirements)
      .map { _ =>
        if (failOnError && run.errors.nonEmpty)
          reportErrors(run.errors, logger)

        run
      }
      .andThen { case _ => infoLoader.cleanAfterRun() }
  }

  private def reportErrors(errors: List[Error], logger: Logger): Unit = {
    require(errors.nonEmpty)

    val maxDisplayErrors = {
      val propName = "org.scalajs.linker.maxlinkingerrors"
      Try(System.getProperty(propName, "20").toInt).getOrElse(20).max(1)
    }

    errors
      .take(maxDisplayErrors)
      .foreach(logError(_, logger, Level.Error))

    val skipped = errors.size - maxDisplayErrors
    if (skipped > 0)
      logger.log(Level.Error, s"Not showing $skipped more linking errors")

    if (initial) {
      throw new LinkingException("There were linking errors")
    } else {
      throw new AssertionError(
          "There were linking errors after the optimizer has run. " +
          "This is a bug, please report it. " +
          "You can work around the bug by disabling the optimizer. " +
          "In the sbt plugin, this can be done with " +
          "`scalaJSLinkerConfig ~= { _.withOptimizer(false) }`.")
    }
  }
}

private class AnalyzerRun(config: CommonPhaseConfig, initial: Boolean,
    infoLoader: InfoLoader)(implicit ec: ExecutionContext) extends Analysis {
  import AnalyzerRun._

  private val allowAddingSyntheticMethods = initial
  private val checkAbstractReachability = initial

  private val isNoModule = config.coreSpec.moduleKind == ModuleKind.NoModule

  private val workTracker: WorkTracker = new WorkTracker
  private[this] val classLoader: ClassLoader = new ClassLoader

  private var objectClassInfo: ClassInfo = _
  private var _classInfos: scala.collection.Map[ClassName, ClassInfo] = _

  def classInfos: scala.collection.Map[ClassName, Analysis.ClassInfo] = _classInfos

  private[this] val _errors = new GrowingList[Error]

  override def errors: List[Error] = _errors.get()

  private val fromAnalyzer = FromCore("analyzer")

  private[this] val _topLevelExportInfos: mutable.Map[(ModuleID, String), TopLevelExportInfo] = emptyThreadSafeMap
  def topLevelExportInfos: scala.collection.Map[(ModuleID, String), Analysis.TopLevelExportInfo] = _topLevelExportInfos

  def computeReachability(moduleInitializers: Seq[ModuleInitializer],
      symbolRequirements: SymbolRequirement): Future[Unit] = {
    loadObjectClass(() => loadEverything(moduleInitializers, symbolRequirements))

    workTracker
      .allowComplete()
      .map(_ => postLoad(moduleInitializers))
  }

  private def loadObjectClass(onSuccess: () => Unit): Unit = {
    implicit val from = fromAnalyzer

    /* Load the java.lang.Object class, and validate it
     * If it is missing or invalid, we're in deep trouble, and cannot continue.
     */
    lookupClass(ObjectClass) { clazz =>
      if (!clazz.nonExistent) {
        objectClassInfo = clazz
        onSuccess()
      }
    }
  }

  private def loadEverything(moduleInitializers: Seq[ModuleInitializer],
      symbolRequirements: SymbolRequirement): Unit = {
    assert(objectClassInfo != null)

    implicit val from = fromAnalyzer

    /* java.lang.Object is always instantiated, because it is the
     * representative class for JS objects.
     */
    objectClassInfo.instantiated()

    /* Hijacked classes are always instantiated, because values of primitive
     * types are their instances.
     *
     * Also, they are part of the core infrastructure. As such, j.l.Object
     * depends on them.
     */
    for (hijacked <- HijackedClasses) {
      lookupClass(hijacked) { clazz =>
        objectClassInfo.addStaticDependency(clazz.className)
        clazz.instantiated()
      }
    }

    // External symbol requirements.
    reachSymbolRequirement(symbolRequirements)

    // Reach entry points
    for (className <- infoLoader.classesWithEntryPoints())
      lookupClass(className)(_.reachEntryPoints())

    // Reach module initializers.
    reachInitializers(moduleInitializers)
  }

  private def postLoad(moduleInitializers: Seq[ModuleInitializer]): Unit = {
    _classInfos = classLoader.loadedInfos()

    if (isNoModule) {
      // Check there is only a single module.
      val publicModuleIDs = (
         _topLevelExportInfos.keys.map(_._1).toList ++
         moduleInitializers.map(i => ModuleID(i.moduleID))
      ).distinct

      if (publicModuleIDs.size > 1)
        _errors ::= MultiplePublicModulesWithoutModuleSupport(publicModuleIDs)
    }

    // Reach additional data, based on reflection methods used
    reachDataThroughReflection()
  }

  private def reachSymbolRequirement(requirement: SymbolRequirement): Unit = {

    /* We use j.l.Object as representation of the core infrastructure.
     * As such, everything depends on j.l.Object and j.l.Object depends on all
     * symbol requirements.
     */

    import SymbolRequirement.Nodes._

    requirement match {
      case AccessModule(origin, moduleName) =>
        implicit val from = FromCore(origin)
        lookupClass(moduleName) { clazz =>
          objectClassInfo.addStaticDependency(clazz.className)
          clazz.accessModule()
        }

      case InstantiateClass(origin, className, constructor) =>
        implicit val from = FromCore(origin)
        lookupClass(className) { clazz =>
          objectClassInfo.addStaticDependency(clazz.className)
          clazz.instantiated()
          clazz.callMethodStatically(MemberNamespace.Constructor, constructor)
        }

      case InstanceTests(origin, className) =>
        implicit val from = FromCore(origin)
        lookupClass(className){ clazz =>
          objectClassInfo.addStaticDependency(clazz.className)
          clazz.useInstanceTests()
        }

      case ClassData(origin, className) =>
        implicit val from = FromCore(origin)
        lookupClass(className) { clazz =>
          objectClassInfo.addStaticDependency(clazz.className)
          clazz.accessData()
        }

      case CallMethod(origin, className, methodName, statically) =>
        implicit val from = FromCore(origin)
        lookupClass(className) { clazz =>
          if (statically) {
            objectClassInfo.addStaticDependency(clazz.className)
            clazz.callMethodStatically(MemberNamespace.Public, methodName)
          } else {
            clazz.callMethod(methodName)
          }
        }

      case CallStaticMethod(origin, className, methodName) =>
        implicit val from = FromCore(origin)
        lookupClass(className) { clazz =>
          objectClassInfo.addStaticDependency(clazz.className)
          clazz.callMethodStatically(MemberNamespace.PublicStatic, methodName)
        }

      case Multiple(requirements) =>
        for (requirement <- requirements)
          reachSymbolRequirement(requirement)

      case NoRequirement => // skip
    }
  }

  private def reachInitializers(
      moduleInitializers: Seq[ModuleInitializer]): Unit = {
    implicit val from = FromCore("module initializers")

    for (moduleInitializer <- moduleInitializers) {
      import ModuleInitializerImpl._

      fromInitializer(moduleInitializer.initializer) match {
        case VoidMainMethod(className, mainMethodName) =>
          lookupClass(className) { classInfo =>
            classInfo.callMethodStatically(MemberNamespace.PublicStatic, mainMethodName)
          }

        case MainMethodWithArgs(className, mainMethodName, _) =>
          lookupClass(className) { classInfo =>
            classInfo.callMethodStatically(MemberNamespace.PublicStatic, mainMethodName)
          }

          // For new Array[String]
          lookupClass(BoxedStringClass)(_.accessData())
      }
    }
  }

  /** Reach additional class data based on reflection methods being used. */
  private def reachDataThroughReflection(): Unit = {

    val classClassInfo = _classInfos.get(ClassClass)

    /* If Class.getSuperclass() is reachable, we can reach the data of all
     * superclasses of classes whose data we can already reach.
     */
    for {
      getSuperclassMethodInfo <-
        classClassInfo.flatMap(_.publicMethodInfos.get(getSuperclassMethodName))
      if getSuperclassMethodInfo.isReachable
    } {
      // calledFrom should always be nonEmpty if isReachable, but let's be robust
      implicit val from =
        getSuperclassMethodInfo.calledFrom.headOption.getOrElse(fromAnalyzer)
      for (classInfo <- _classInfos.values.filter(_.isDataAccessed).toList) {
        @tailrec
        def loop(classInfo: ClassInfo): Unit = {
          classInfo.accessData()
          classInfo.superClass match {
            case Some(superClass) =>
              classInfo.addStaticDependency(superClass.className)
              loop(superClass)

            case None =>
          }
        }
        loop(classInfo)
      }
    }
  }

  private def lookupClass(className: ClassName)(
      onSuccess: ClassInfo => Unit)(implicit from: From): Unit = {
    workTracker.track {
      classLoader.lookupClass(className).map {
        case info: ClassInfo =>
          info.link()
          onSuccess(info)

        case CycleInfo(cycle, root) =>
          assert(root == null, s"unresolved root: $root")
          _errors ::= CycleInInheritanceChain(cycle, fromAnalyzer)
      }
    }
  }

  private final class ClassLoader(implicit ec: ExecutionContext) {
    private[this] val _classInfos = emptyThreadSafeMap[ClassName, ClassLoadingState]

    def lookupClass(className: ClassName): Future[LoadingResult] = {
      ensureLoading(className) match {
        case loading: LoadingClass => loading.result
        case info: ClassInfo       => Future.successful(info)
      }
    }

    def loadedInfos(): scala.collection.Map[ClassName, ClassInfo] = {
      // Assemble loaded infos.
      val infos = _classInfos.collect { case (k, i: ClassInfo) => (k, i) }

      assert(_errors.get().nonEmpty || infos.size == _classInfos.size,
        "unloaded classes in post load phase")

      infos
    }

    private def lookupClassForLinking(className: ClassName,
        origin: LoadingClass): Future[LoadingResult] = {
      ensureLoading(className) match {
        case loading: LoadingClass => loading.requestLink(origin)
        case info: ClassInfo       => Future.successful(info)
      }
    }

    private def ensureLoading(className: ClassName): ClassLoadingState = {
      var loading: LoadingClass = null
      val state = _classInfos.getOrElseUpdate(className, {
        loading = new LoadingClass(className)
        loading
      })

      if (state eq loading) {
        // We just added `loading`, actually load.
        val maybeInfo = infoLoader.loadInfo(className)
        val info = maybeInfo.getOrElse {
          Future.successful(createMissingClassInfo(className))
        }

        val result = info.flatMap { data =>
          doLoad(data, loading, nonExistent = maybeInfo.isEmpty)
        }

        loading.completeWith(result)
      }

      state
    }

    private def doLoad(data: Infos.ClassInfo, origin: LoadingClass,
        nonExistent: Boolean): Future[LoadingResult] = {
      val className = data.className

      for {
        maybeAncestors <- Future.traverse(data.superClass.toList ++ data.interfaces)(
            lookupClassForLinking(_, origin))
      } yield {
        val maybeCycle = maybeAncestors.collectFirst {
          case cycle @ CycleInfo(_, null) => cycle

          case CycleInfo(c, root) if root == className =>
            CycleInfo(className :: c, null)

          case CycleInfo(c, root) =>
            CycleInfo(className :: c, root)
        }

        maybeCycle.getOrElse {
          val ancestors = maybeAncestors.asInstanceOf[List[ClassInfo]]

          val (superClass, interfaces) =
            if (data.superClass.isEmpty) (None, ancestors)
            else (Some(ancestors.head), ancestors.tail)

          val info = new ClassInfo(data, superClass, interfaces, nonExistent)

          _classInfos.put(className, info)

          implicit val from = FromClass(info)
          ancestors.foreach(_.link())

          info
        }
      }
    }
  }

  private sealed trait LoadingResult
  private sealed trait ClassLoadingState

  // sealed instead of final because of spurious unchecked warnings
  private sealed case class CycleInfo(cycle: List[ClassName], root: ClassName)
      extends LoadingResult

  private final class LoadingClass(className: ClassName)
      extends ClassLoadingState {

    private val promise = Promise[LoadingResult]()
    private val knownDescendants = emptyThreadSafeMap[LoadingClass, Unit]

    knownDescendants.update(this, ())

    def requestLink(origin: LoadingClass): Future[LoadingResult] = {
      if (origin.knownDescendants.contains(this)) {
        Future.successful(CycleInfo(Nil, className))
      } else {
        this.knownDescendants ++= origin.knownDescendants
        promise.future
      }
    }

    def result: Future[LoadingResult] = promise.future

    def completeWith(result: Future[LoadingResult]): Unit =
      promise.completeWith(result)
  }

  private sealed trait ModuleUnit {
    def addStaticDependency(clazz: ClassName): Unit
    def addExternalDependency(module: String): Unit
    def addDynamicDependency(clazz: ClassName): Unit
  }

  private class ClassInfo(
      val data: Infos.ClassInfo,
      unvalidatedSuperClass: Option[ClassInfo],
      unvalidatedInterfaces: List[ClassInfo],
      val nonExistent: Boolean)
      extends Analysis.ClassInfo with ClassLoadingState with LoadingResult with ModuleUnit {

    private[this] val _linkedFrom = new GrowingList[From]
    def linkedFrom: List[From] = _linkedFrom.get()

    val className = data.className
    val kind = data.kind
    val isAnyModuleClass =
      data.kind.hasModuleAccessor || data.kind == ClassKind.NativeJSModuleClass
    val isInterface = data.kind == ClassKind.Interface
    val isScalaClass = data.kind.isClass || data.kind == ClassKind.HijackedClass
    val isJSClass = data.kind.isJSClass
    val isJSType = data.kind.isJSType
    val isAnyClass = isScalaClass || isJSClass
    val isNativeJSClass =
      kind == ClassKind.NativeJSClass || kind == ClassKind.NativeJSModuleClass

    val superClass: Option[ClassInfo] =
      validateSuperClass(unvalidatedSuperClass)

    val interfaces: List[ClassInfo] =
      validateInterfaces(unvalidatedInterfaces)

    /** Ancestors of this class or interface.
     *
     *  This always includes this class and `java.lang.Object`.
     */
    val ancestors: List[ClassInfo] = {
      if (className == ObjectClass) {
        this :: Nil
      } else {
        val parents = superClass.getOrElse(objectClassInfo) :: interfaces
        this +: parents.flatMap(_.ancestors).distinct
      }
    }

    def link()(implicit from: From): Unit = {
      if (nonExistent)
        _errors ::= MissingClass(this, from)

      _linkedFrom ::= from
    }

    private[this] def validateSuperClass(superClass: Option[ClassInfo]): Option[ClassInfo] = {
      def from = FromClass(this)

      kind match {
        case _ if className == ObjectClass =>
          assert(superClass.isEmpty)

          None

        case ClassKind.Class | ClassKind.ModuleClass | ClassKind.HijackedClass =>
          val superCl = superClass.get // checked by ClassDef checker.
          if (superCl.kind != ClassKind.Class) {
            _errors ::= InvalidSuperClass(superCl, this, from)
            Some(objectClassInfo)
          } else {
            superClass
          }

        case ClassKind.Interface =>
          assert(superClass.isEmpty)

          None

        case ClassKind.JSClass | ClassKind.JSModuleClass =>
          /* There is no correct fallback in case of error, here. The logical
           * thing to do would be to pick `js.Object`, but we cannot be sure
           * that `js.Object` and its inheritance chain are valid themselves.
           * So we just say superClass = None in invalid cases, and make sure
           * this does not blow up the rest of the analysis.
           */
          val superCl = superClass.get // checked by ClassDef checker.
          superCl.kind match {
            case ClassKind.JSClass | ClassKind.NativeJSClass =>
              superClass // ok
            case _ =>
              _errors ::= InvalidSuperClass(superCl, this, from)
              None
          }

        case ClassKind.NativeJSClass | ClassKind.NativeJSModuleClass =>
          val superCl = superClass.get // checked by ClassDef checker.
          superCl.kind match {
            case ClassKind.JSClass | ClassKind.NativeJSClass =>
              superClass // ok
            case _ if superCl eq objectClassInfo =>
              superClass // ok
            case _ =>
              _errors ::= InvalidSuperClass(superCl, this, from)
              Some(objectClassInfo)
          }

        case ClassKind.AbstractJSType =>
          superClass.flatMap { superCl =>
            superCl.kind match {
              case ClassKind.JSClass | ClassKind.NativeJSClass =>
                superClass // ok
              case _ if superCl eq objectClassInfo =>
                superClass // ok
              case _ =>
                _errors ::= InvalidSuperClass(superCl, this, from)
                None
            }
          }
      }
    }

    private[this] def validateInterfaces(interfaces: List[ClassInfo]): List[ClassInfo] = {
      def from = FromClass(this)

      val validSuperIntfKind = kind match {
        case ClassKind.Class | ClassKind.ModuleClass |
            ClassKind.HijackedClass | ClassKind.Interface =>
          ClassKind.Interface
        case ClassKind.JSClass | ClassKind.JSModuleClass |
            ClassKind.NativeJSClass | ClassKind.NativeJSModuleClass |
            ClassKind.AbstractJSType =>
          ClassKind.AbstractJSType
      }

      interfaces.filter { superIntf =>
        if (superIntf.nonExistent) {
          // Remove it but do not report an additional error message
          false
        } else if (superIntf.kind != validSuperIntfKind) {
          _errors ::= InvalidImplementedInterface(superIntf, this, from)
          false
        } else {
          true
        }
      }
    }

    private[this] val _isInstantiated = new AtomicBoolean(false)
    def isInstantiated: Boolean = _isInstantiated.get()

    private[this] val _isAnySubclassInstantiated = new AtomicBoolean(false)
    def isAnySubclassInstantiated: Boolean = _isAnySubclassInstantiated.get()

    private[this] val isModuleAccessed = new AtomicBoolean(false)

    private[this] val _areInstanceTestsUsed = new AtomicBoolean(false)
    def areInstanceTestsUsed: Boolean = _areInstanceTestsUsed.get()

    private[this] val _isDataAccessed = new AtomicBoolean(false)
    def isDataAccessed: Boolean = _isDataAccessed.get()

    private[this] val _fieldsRead: mutable.Map[FieldName, Unit] = emptyThreadSafeMap
    private[this] val _fieldsWritten: mutable.Map[FieldName, Unit] = emptyThreadSafeMap
    val _staticFieldsRead: mutable.Map[FieldName, Unit] = emptyThreadSafeMap
    val _staticFieldsWritten: mutable.Map[FieldName, Unit] = emptyThreadSafeMap

    def fieldsRead: scala.collection.Set[FieldName] = _fieldsRead.keySet
    def fieldsWritten: scala.collection.Set[FieldName] = _fieldsWritten.keySet
    def staticFieldsRead: scala.collection.Set[FieldName] = _staticFieldsRead.keySet
    def staticFieldsWritten: scala.collection.Set[FieldName] = _staticFieldsWritten.keySet

    private[this] val _jsNativeMembersUsed: mutable.Map[MethodName, Unit] = emptyThreadSafeMap
    def jsNativeMembersUsed: scala.collection.Set[MethodName] = _jsNativeMembersUsed.keySet

    val jsNativeLoadSpec: Option[JSNativeLoadSpec] = data.jsNativeLoadSpec

    private[this] val _staticDependencies: mutable.Map[ClassName, Unit] = emptyThreadSafeMap
    private[this] val _externalDependencies: mutable.Map[String, Unit] = emptyThreadSafeMap
    private[this] val _dynamicDependencies: mutable.Map[ClassName, Unit] = emptyThreadSafeMap

    def addStaticDependency(clazz: ClassName): Unit = _staticDependencies.update(clazz, ())
    def addExternalDependency(module: String): Unit = _externalDependencies.update(module, ())
    def addDynamicDependency(clazz: ClassName): Unit = _dynamicDependencies.update(clazz, ())

    def staticDependencies: scala.collection.Set[ClassName] = _staticDependencies.keySet
    def externalDependencies: scala.collection.Set[String] = _externalDependencies.keySet
    def dynamicDependencies: scala.collection.Set[ClassName] = _dynamicDependencies.keySet

    /* j.l.Object represents the core infrastructure. As such, everything
     * depends on it unconditionally.
     */
    if (className != ObjectClass)
      addStaticDependency(ObjectClass)

    private[this] val _instantiatedFrom = new GrowingList[From]
    def instantiatedFrom: List[From] = _instantiatedFrom.get()

    private[this] val _dispatchCalledFrom: mutable.Map[MethodName, GrowingList[From]] = emptyThreadSafeMap
    def dispatchCalledFrom(methodName: MethodName): Option[List[From]] =
      _dispatchCalledFrom.get(methodName).map(_.get())

    /** Methods that have been called on this interface.
     *
     *  Note that we maintain the invariant
     *
     *  methodsCalledLog.toSet == dispatchCalledFrom.keySet.
     *
     *  This is because we need to be able to snapshot methodsCalledLog in
     *  subclassInstantiated. TrieMap would support snapshotting, but a plain
     *  mutable.Map doesn't (so it wouldn't cross compile to JS).
     */
    private val methodsCalledLog = new GrowingList[MethodName]

    /** List of all instantiated (Scala) subclasses of this Scala class/trait.
     *  For JS types, this always remains empty.
     */
    private val _instantiatedSubclasses = new GrowingList[ClassInfo]

    private val nsMethodInfos = Array.tabulate(MemberNamespace.Count) { nsOrdinal =>
      val namespace = MemberNamespace.fromOrdinal(nsOrdinal)

      val m = emptyThreadSafeMap[MethodName, MethodInfo]

      for ((name, data) <- data.methods(nsOrdinal))
        m.put(name, new MethodInfo(this, namespace, name, data))

      m
    }

    def methodInfos(
        namespace: MemberNamespace): mutable.Map[MethodName, MethodInfo] = {
      nsMethodInfos(namespace.ordinal)
    }

    val publicMethodInfos: mutable.Map[MethodName, MethodInfo] =
      methodInfos(MemberNamespace.Public)

    def lookupAbstractMethod(methodName: MethodName): MethodInfo = {
      val candidatesIterator = for {
        ancestor <- ancestors.iterator
        m <- ancestor.publicMethodInfos.get(methodName)
        if !m.isDefaultBridge && (!m.nonExistent || ancestor == this)
      } yield {
        m
      }

      if (candidatesIterator.isEmpty)
        createNonExistentPublicMethod(methodName)
      else
        candidatesIterator.next()
    }

    def lookupMethod(methodName: MethodName): MethodInfo = {
      tryLookupMethod(methodName).getOrElse {
        createNonExistentPublicMethod(methodName)
      }
    }

    private def createNonExistentPublicMethod(methodName: MethodName): MethodInfo = {
      /* Use getOrElseUpdate to avoid overriding an abstract method:
       * When being called from lookupMethod, it is possible that an abstract
       * method exists.
       */
      publicMethodInfos.getOrElseUpdate(methodName, {
        val syntheticData = makeSyntheticMethodInfo()
        new MethodInfo(this, MemberNamespace.Public, methodName, syntheticData, nonExistent = true)
      })
    }

    def tryLookupMethod(methodName: MethodName): Option[MethodInfo] = {
      assert(isScalaClass || isInterface,
          s"Cannot call lookupMethod($methodName) on non Scala class $this")

      publicMethodInfos.get(methodName) match {
        case Some(m) if !m.isAbstract => Some(m)

        case _ =>
          val candidate = superClass
            .flatMap(_.tryLookupMethod(methodName))
            .filterNot(_.nonExistent)

          if (allowAddingSyntheticMethods) {
            def maybeDefaultTarget = getDefaultTarget(methodName)

            def needsDefaultOverride(method: MethodInfo): Boolean = {
              /* The .get is OK, since we only get here if:
               * - This class doesn't implement the method directly.
               * - The superClass has found a default target.
               * In this case, we always find at least one target.
               */
              method.isDefaultBridge && method.defaultBridgeTarget != maybeDefaultTarget.get.owner.className
            }

            candidate
              .filterNot(needsDefaultOverride(_))
              .orElse(maybeDefaultTarget.map(createDefaultBridge(_)))
          } else {
            candidate
          }
      }
    }

    private val defaultTargets = emptyThreadSafeMap[MethodName, Option[MethodInfo]]

    private def getDefaultTarget(methodName: MethodName): Option[MethodInfo] =
      defaultTargets.getOrElseUpdate(methodName, findDefaultTarget(methodName))

    /** Resolves an inherited default method.
     *
     *  This lookup is specified by the JVM resolution rules for default
     *  methods. See the `invokespecial` opcode in the JVM Specification
     *  version 8, Section 6.5:
     *  https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.invokespecial
     */
    private def findDefaultTarget(methodName: MethodName): Option[MethodInfo] = {
      val candidates = for {
        intf <- ancestors if intf.isInterface
        m <- intf.publicMethodInfos.get(methodName)
        if !m.isAbstract && !m.isDefaultBridge && !m.nonExistent
      } yield m

      val notShadowed = candidates filterNot { m =>
        candidates exists { n =>
          (n ne m) && n.owner.ancestors.contains(m.owner)
        }
      }

      if (notShadowed.size > 1) {
        /* Deviation from the spec: if there are several targets, the spec
         * chooses one arbitrarily. However, unless the classpath is
         * manipulated and/or corrupted, this should not happen. The Java
         * *language* and compiler do not let this happen on their own.
         * Besides, the current implementation of the JVM throws an
         * IncompatibleClassChangeError when trying to resolve such ambiguous
         * references.
         * So we emit an error too, so that we can more easily discover bugs.
         * We use fromAnalyzer because we don't have any From here (we
         * shouldn't, since lookup methods are not supposed to produce errors).
         */
        _errors ::= ConflictingDefaultMethods(notShadowed, fromAnalyzer)
      }

      notShadowed.headOption
    }

    private def createDefaultBridge(target: MethodInfo): MethodInfo = {
      val methodName = target.methodName

      publicMethodInfos.getOrElseUpdate(methodName, {
        val targetOwner = target.owner

        val syntheticInfo = makeSyntheticMethodInfo(
            methodsCalledStatically = List(
                targetOwner.className -> NamespacedMethodName(MemberNamespace.Public, methodName)))
        new MethodInfo(this, MemberNamespace.Public, methodName, syntheticInfo,
            syntheticKind = MethodSyntheticKind.DefaultBridge(targetOwner.className))
      })
    }

    private def maybeReachReflProxyMethod(proxyName: MethodName)(implicit from: From): Unit = {
      if (!allowAddingSyntheticMethods) {
        tryLookupMethod(proxyName).foreach(_.reach(this))
      } else {
        publicMethodInfos
          .get(proxyName)
          .fold(findAndReachReflectiveTarget(proxyName))(_.reach(this))
      }
    }

    private def findAndReachReflectiveTarget(
        proxyName: MethodName)(implicit from: From): Unit = {
      /* The lookup for a target method in this code implements the
       * algorithm defining `java.lang.Class.getMethod`. This mimics how
       * reflective calls are implemented on the JVM, at link time.
       *
       * We add a bit of guess-work for default methods, as the documentation
       * is very vague about them. Basically, we just take the first match in
       * `ancestors`, as it's easy, and we're in a gray area anyway. At least,
       * this will work when there is no overload.
       *
       * Caveat: protected methods are not ignored. This can only make an
       * otherwise invalid reflective call suddenly able to call a protected
       * method. It never breaks valid reflective calls. This could be fixed
       * if the IR retained the information that a method is protected.
       */

      @tailrec
      def findFirstNonEmptyCandidates(ancestors: List[ClassInfo]): List[MethodInfo] = {
        ancestors match {
          case ancestor :: nextAncestors =>
            val candidates = ancestor.findProxyCandidates(proxyName)
            if (candidates.isEmpty)
              findFirstNonEmptyCandidates(nextAncestors)
            else
              candidates
          case Nil =>
            Nil
        }
      }

      val candidates = findFirstNonEmptyCandidates(ancestorsInReflectiveTargetOrder)

      candidates match {
        case Nil =>
          ()

        case onlyCandidate :: Nil =>
          // Fast path that does not require workTracker.track
          createReflProxy(proxyName, onlyCandidate.methodName).reach(this)

        case _ =>
          val future = for {
            reflectiveTarget <- computeMostSpecificProxyMatch(candidates)
          } yield {
            createReflProxy(proxyName, reflectiveTarget.methodName).reach(this)
          }

          workTracker.track(future)
      }
    }

    private lazy val ancestorsInReflectiveTargetOrder: List[ClassInfo] = {
      val b = new mutable.ListBuffer[ClassInfo]

      @tailrec
      def addSuperClasses(superClass: ClassInfo): Unit = {
        b += superClass
        superClass.superClass match {
          case Some(next) => addSuperClasses(next)
          case None       => ()
        }
      }
      addSuperClasses(this)

      b.prependToList(ancestors.filter(_.isInterface))
    }

    private def findProxyCandidates(proxyName: MethodName): List[MethodInfo] =
      proxyCandidates.getOrElse(proxyName, Nil)

    private lazy val proxyCandidates = {
      val result = mutable.Map.empty[MethodName, List[MethodInfo]]
      val iter = publicMethodInfos.valuesIterator
      while (iter.hasNext) {
        val m = iter.next()
        val include = {
          // TODO In theory we should filter out protected methods
          !m.isReflectiveProxy && !m.isDefaultBridge && !m.isAbstract && !m.nonExistent
        }
        if (include) {
          val proxyName = MethodName.reflectiveProxy(m.methodName.simpleName, m.methodName.paramTypeRefs)
          val prev = result.getOrElse(proxyName, Nil)
          result.update(proxyName, m :: prev)
        }
      }
      result
    }

    private def computeMostSpecificProxyMatch(candidates: List[MethodInfo])(
        implicit from: From): Future[MethodInfo] = {

      /* From the JavaDoc of java.lang.Class.getMethod:
       *
       *   If more than one [candidate] method is found in C, and one of these
       *   methods has a return type that is more specific than any of the
       *   others, that method is reflected; otherwise one of the methods is
       *   chosen arbitrarily.
       */

      def ifMostSpecific(candidate: MethodInfo): Future[Option[MethodInfo]] = {
        val specificityChecks = for {
          otherCandidate <- candidates
          if candidate != otherCandidate
        } yield {
          isMoreSpecific(otherCandidate.methodName.resultTypeRef,
              candidate.methodName.resultTypeRef)
        }

        for {
          moreSpecific <- Future.find(specificityChecks)(identity)
        } yield {
          if (moreSpecific.isEmpty) Some(candidate)
          else None
        }
      }

      val specificCandidates = candidates.map(ifMostSpecific)

      /* This last step (chosen arbitrarily) causes some soundness issues of
       * the implementation of reflective calls. This is bug-compatible with
       * Scala/JVM.
       */
      for {
        candidate <- Future.find(specificCandidates)(_.nonEmpty)
      } yield {
        /* First get: There must be a most specific candidate.
         * Second get: That's our find condition from above.
         */
        candidate.get.get
      }
    }

    private def reflProxyMatches(methodName: MethodName,
        proxyName: MethodName): Boolean = {
      methodName.simpleName == proxyName.simpleName &&
      methodName.paramTypeRefs == proxyName.paramTypeRefs
    }

    private def isMoreSpecific(left: ir.Types.TypeRef, right: ir.Types.TypeRef)(
        implicit from: From): Future[Boolean] = {
      import ir.Types._

      def classIsMoreSpecific(leftCls: ClassName, rightCls: ClassName): Future[Boolean] = {
        if (leftCls == rightCls) {
          Future.successful(false)
        } else {
          val promise = Promise[Boolean]()

          lookupClass(leftCls) { leftInfo =>
            lookupClass(rightCls) { rightInfo =>
              promise.success(leftInfo.ancestors.contains(rightInfo))
            }
          }

          promise.future
        }
      }

      (left, right) match {
        case (ClassRef(leftCls), ClassRef(rightCls)) =>
          classIsMoreSpecific(leftCls, rightCls)
        case (ArrayTypeRef(ClassRef(leftBaseCls), leftDepth),
            ArrayTypeRef(ClassRef(rightBaseCls), rightDepth)) =>
          if (leftDepth != rightDepth) Future.successful(false)
          else classIsMoreSpecific(leftBaseCls, rightBaseCls)
        case (ArrayTypeRef(_, _), ClassRef(ObjectClass)) =>
          Future.successful(true)
        case _ =>
          Future.successful(false)
      }
    }

    private def createReflProxy(proxyName: MethodName,
        targetName: MethodName): MethodInfo = {
      assert(this.isScalaClass,
          s"Cannot create reflective proxy in non-Scala class $this")

      publicMethodInfos.getOrElseUpdate(proxyName, {
        val syntheticInfo = makeSyntheticMethodInfo(
            methodsCalled = List(this.className -> targetName))
        new MethodInfo(this, MemberNamespace.Public, proxyName, syntheticInfo,
            syntheticKind = MethodSyntheticKind.ReflectiveProxy(targetName))
      })
    }

    def lookupStaticLikeMethod(namespace: MemberNamespace,
        methodName: MethodName): MethodInfo = {
      assert(namespace != MemberNamespace.Public)

      methodInfos(namespace).getOrElseUpdate(methodName, {
        val syntheticData = makeSyntheticMethodInfo()
        new MethodInfo(this, namespace, methodName, syntheticData, nonExistent = true)
      })
    }

    def tryLookupStaticLikeMethod(namespace: MemberNamespace,
        methodName: MethodName): Option[MethodInfo] = {
      assert(namespace != MemberNamespace.Public)
      methodInfos(namespace).get(methodName)
    }

    override def toString(): String = className.nameString

    def reachEntryPoints(): Unit = {
      implicit val from = FromExports

      // Static initializer
      tryLookupStaticLikeMethod(MemberNamespace.StaticConstructor,
          StaticInitializerName).foreach {
        _.reachStatic()(fromAnalyzer)
      }

      // Top Level Exports
      for (tle <- data.topLevelExports) {
        val key = (tle.moduleID, tle.exportName)
        val info = new TopLevelExportInfo(className, tle)
        info.reach()

        _topLevelExportInfos.put(key, info).foreach { other =>
          _errors ::= ConflictingTopLevelExport(tle.moduleID, tle.exportName, List(info, other))
        }
      }
    }

    def accessModule()(implicit from: From): Unit = {
      if (!isAnyModuleClass) {
        _errors ::= NotAModule(this, from)
      } else if (!isModuleAccessed.getAndSet(true)) {
        instantiated() // TODO: Shouldn't we always add the from?
        if (isScalaClass)
          callMethodStatically(MemberNamespace.Constructor, NoArgConstructorName)
      }
    }

    def instantiated()(implicit from: From): Unit = {
      _instantiatedFrom ::= from

      if (!(isScalaClass || isJSClass || isNativeJSClass)) {
        /* Ignore.
         * TODO? Shouldn't this be a linking error
         * instead?
         */
      } else if (!_isInstantiated.getAndSet(true)) {

        // TODO: Why is this not in subclassInstantiated()?
        fieldsRead.foreach(referenceFieldClasses(_))
        fieldsWritten.foreach(referenceFieldClasses(_))

        if (isScalaClass) {
          accessData()

          /* First mark the ancestors as subclassInstantiated() then fetch the
           * methodsCalledLog, for all ancestors. This order is important to
           * ensure that concurrently analyzed method calls work correctly.
           *
           * Further, we only actually perform the resolved calls once we have
           * fetched all the logs. This is to minimize duplicate work:
           * during the resolved calls, new methods could be called and added
           * to the log; they will already see the new subclasses so we should
           * *not* see them in the logs, lest we perform that work twice.
           */

          val allMethodsCalledLogs = for (ancestor <- ancestors) yield {
            ancestor.subclassInstantiated()
            ancestor._instantiatedSubclasses ::= this
            ancestor -> ancestor.methodsCalledLog.get()
          }

          for {
            (ancestor, ancestorLog) <- allMethodsCalledLogs
            methodName <- ancestorLog
          } {
            implicit val from = FromDispatch(ancestor, methodName)
            callMethodResolved(methodName)
          }
        } else {
          assert(isJSClass || isNativeJSClass)

          subclassInstantiated()

          if (isJSClass) {
            superClass.foreach(_.instantiated())
            tryLookupStaticLikeMethod(MemberNamespace.StaticConstructor,
                ClassInitializerName).foreach {
              staticInit => staticInit.reachStatic()
            }
          } else {
            for (jsNativeLoadSpec <- data.jsNativeLoadSpec)
              validateLoadSpec(jsNativeLoadSpec, jsNativeMember = None)
          }

          for (reachabilityInfo <- data.jsMethodProps)
            followReachabilityInfo(reachabilityInfo, this)(FromExports)
        }
      }
    }

    private def subclassInstantiated()(implicit from: From): Unit = {
      _instantiatedFrom ::= from

      if (!_isAnySubclassInstantiated.getAndSet(true)) {
        if (!isInterface) {
          if (!isNativeJSClass) {
            for (clazz <- superClass) {
              if (clazz.isNativeJSClass)
                clazz.jsNativeLoadSpec.foreach(addLoadSpec(this, _))
              else
                addStaticDependency(clazz.className)
            }
          }

          // Reach exported members
          if (!isJSClass) {
            for (reachabilityInfo <- data.jsMethodProps)
              followReachabilityInfo(reachabilityInfo, this)(FromExports)
          }
        }
      }
    }

    def useInstanceTests()(implicit from: From): Unit = {
      _areInstanceTestsUsed.set(true)
    }

    def accessData()(implicit from: From): Unit = {
      if (!_isDataAccessed.getAndSet(true)) {
        // #4548 The `isInstance` function will refer to the class value
        if (kind == ClassKind.NativeJSClass)
          jsNativeLoadSpec.foreach(addLoadSpec(this, _))
      }
    }

    def callMethod(methodName: MethodName)(implicit from: From): Unit = {
      /* First add the call to the log, then fetch the instantiated subclasses,
       * then perform the resolved call. This order is important because,
       * during the resolved calls, new instantiated subclasses could be
       * detected, and those need to see the updated log, since the loop in
       * this method won't see them.
       */

      val froms = _dispatchCalledFrom.getOrElseUpdate(methodName, new GrowingList)

      if (froms.addIfNil(from)) {
        // New call.
        val fromDispatch = FromDispatch(this, methodName)

        methodsCalledLog ::= methodName
        val subclasses = _instantiatedSubclasses.get()
        for (subclass <- subclasses)
          subclass.callMethodResolved(methodName)(fromDispatch)

        if (checkAbstractReachability) {
          /* Also lookup the method as abstract from this class, to make sure it
           * is *declared* on this type. We do this after the concrete lookup to
           * avoid work, since a concretely reachable method is already marked as
           * abstractly reachable.
           */
          if (!methodName.isReflectiveProxy)
            lookupAbstractMethod(methodName).reachAbstract()(fromDispatch)
        }
      } else {
        // Already called before; add the new from
        froms ::= from
      }
    }

    private def callMethodResolved(methodName: MethodName)(
        implicit from: From): Unit = {
      if (methodName.isReflectiveProxy) {
        maybeReachReflProxyMethod(methodName)
      } else {
        lookupMethod(methodName).reach(this)
      }
    }

    def callMethodStatically(namespace: MemberNamespace,
        methodName: MethodName)(
        implicit from: From): Unit = {
      assert(!methodName.isReflectiveProxy,
          s"Trying to call statically refl proxy $this.$methodName")
      if (namespace != MemberNamespace.Public)
        lookupStaticLikeMethod(namespace, methodName).reachStatic()
      else
        lookupMethod(methodName).reachStatic()
    }

    def reachField(info: Infos.FieldReachable)(implicit from: From): Unit = {
      val fieldName = info.fieldName
      if (info.read)
        _fieldsRead.update(fieldName, ())
      if (info.written)
        _fieldsWritten.update(fieldName, ())
      if (isInstantiated)
        referenceFieldClasses(fieldName)
    }

    def useJSNativeMember(name: MethodName)(
        implicit from: From): Option[JSNativeLoadSpec] = {
      val maybeJSNativeLoadSpec = data.jsNativeMembers.get(name)
      if (_jsNativeMembersUsed.put(name, ()).isEmpty) {
        maybeJSNativeLoadSpec match {
          case None =>
            _errors ::= MissingJSNativeMember(this, name, from)
          case Some(jsNativeLoadSpec) =>
            validateLoadSpec(jsNativeLoadSpec, Some(name))
        }
      }
      maybeJSNativeLoadSpec
    }

    private def referenceFieldClasses(fieldName: FieldName)(implicit from: From): Unit = {
      assert(isInstantiated)

      /* Reach referenced classes of non-static fields
       *
       * We do not need to add this to staticDependencies: The definition
       * site will not reference the classes in the final JS code.
       */
      for {
        className <- data.referencedFieldClasses.get(fieldName)
      } {
        lookupClass(className)(_ => ())
      }
    }

    private def validateLoadSpec(jsNativeLoadSpec: JSNativeLoadSpec,
        jsNativeMember: Option[MethodName])(implicit from: From): Unit = {
      if (isNoModule) {
        jsNativeLoadSpec match {
          case JSNativeLoadSpec.Import(module, _) =>
            _errors ::= ImportWithoutModuleSupport(module, this, jsNativeMember, from)
          case _ =>
        }
      }
    }
  }

  private class MethodInfo(
    val owner: ClassInfo,
    val namespace: MemberNamespace,
    val methodName: MethodName,
    data: Infos.MethodInfo,
    val nonExistent: Boolean = false,
    val syntheticKind: MethodSyntheticKind = MethodSyntheticKind.None
  ) extends Analysis.MethodInfo {

    val isAbstract = data.isAbstract

    private[this] val _isAbstractReachable = new AtomicBoolean(false)
    def isAbstractReachable: Boolean = _isAbstractReachable.get()

    private[this] val _isReachable = new AtomicBoolean(false)
    def isReachable: Boolean = _isReachable.get()

    private[this] val _calledFrom = new GrowingList[From]
    def calledFrom: List[From] = _calledFrom.get()

    private[this] val _instantiatedSubclasses = new GrowingList[ClassInfo]
    def instantiatedSubclasses: List[ClassInfo] = _instantiatedSubclasses.get()

    def isReflectiveProxy: Boolean =
      methodName.isReflectiveProxy

    def isDefaultBridge: Boolean =
      syntheticKind.isInstanceOf[MethodSyntheticKind.DefaultBridge]

    /** Throws MatchError if `!isDefaultBridge`. */
    def defaultBridgeTarget: ClassName = (syntheticKind: @unchecked) match {
      case MethodSyntheticKind.DefaultBridge(target) => target
    }

    override def toString(): String =
      s"$owner.${methodName.simpleName.nameString}"

    def reachStatic()(implicit from: From): Unit = {
      checkConcrete()

      _calledFrom ::= from
      if (!_isReachable.getAndSet(true)) {
        _isAbstractReachable.set(true)
        doReach()
      }
    }

    def reachAbstract()(implicit from: From): Unit = {
      assert(namespace == MemberNamespace.Public)

      if (!_isAbstractReachable.getAndSet(true)) {
        checkExistent()
        _calledFrom ::= from
      }
    }

    def reach(inClass: ClassInfo)(implicit from: From): Unit = {
      assert(!namespace.isStatic,
          s"Trying to dynamically reach the static method $this")
      assert(owner.isAnyClass,
          s"Trying to dynamically reach the non-class method $this")
      assert(!namespace.isConstructor,
          s"Trying to dynamically reach the constructor $this")

      checkConcrete()

      _calledFrom ::= from
      _instantiatedSubclasses ::= inClass

      if (!_isReachable.getAndSet(true)) {
        _isAbstractReachable.set(true)
        doReach()
      }
    }

    private def checkExistent()(implicit from: From) = {
      if (nonExistent)
        _errors ::= MissingMethod(this, from)
    }

    private def checkConcrete()(implicit from: From) = {
      if (nonExistent || isAbstract)
        _errors ::= MissingMethod(this, from)
    }

    private[this] def doReach(): Unit =
      followReachabilityInfo(data, owner)(FromMethod(this))
  }

  private class TopLevelExportInfo(val owningClass: ClassName, data: Infos.TopLevelExportInfo)
      extends Analysis.TopLevelExportInfo with ModuleUnit {
    val moduleID: ModuleID = data.moduleID
    val exportName: String = data.exportName

    if (isNoModule && !ir.Trees.JSGlobalRef.isValidJSGlobalRefName(exportName)) {
      _errors ::= InvalidTopLevelExportInScript(this)
    }

    private[this] val _staticDependencies: mutable.Map[ClassName, Unit] = emptyThreadSafeMap
    private[this] val _externalDependencies: mutable.Map[String, Unit] = emptyThreadSafeMap

    def addStaticDependency(clazz: ClassName): Unit = _staticDependencies.update(clazz, ())
    def addExternalDependency(module: String): Unit = _externalDependencies.update(module, ())
    def addDynamicDependency(clazz: ClassName): Unit = {
      throw new AssertionError("dynamic dependency for top level export " +
          s"$moduleID.$exportName (owned by $owningClass) on $clazz")
    }

    def staticDependencies: scala.collection.Set[ClassName] = _staticDependencies.keySet
    def externalDependencies: scala.collection.Set[String] = _externalDependencies.keySet

    def reach(): Unit = followReachabilityInfo(data.reachability, this)(FromExports)
  }

  private def followReachabilityInfo(data: ReachabilityInfo, moduleUnit: ModuleUnit)(
      implicit from: From): Unit = {

    def addInstanceDependency(info: ClassInfo) = {
      info.jsNativeLoadSpec.foreach(addLoadSpec(moduleUnit, _))
      if (info.kind.isAnyNonNativeClass)
        moduleUnit.addStaticDependency(info.className)
    }

    for (dataInClass <- data.byClass) {
      lookupClass(dataInClass.className) { clazz =>
        val className = dataInClass.className

        val flags = dataInClass.flags
        if (flags != 0) {
          if ((flags & ReachabilityInfoInClass.FlagModuleAccessed) != 0) {
            clazz.accessModule()
            addInstanceDependency(clazz)
          }

          if ((flags & ReachabilityInfoInClass.FlagInstantiated) != 0) {
            clazz.instantiated()
            addInstanceDependency(clazz)
          }

          if ((flags & ReachabilityInfoInClass.FlagInstanceTestsUsed) != 0) {
            moduleUnit.addStaticDependency(className)
            clazz.useInstanceTests()
          }

          if ((flags & ReachabilityInfoInClass.FlagClassDataAccessed) != 0) {
            moduleUnit.addStaticDependency(className)
            clazz.accessData()
          }

          if ((flags & ReachabilityInfoInClass.FlagStaticallyReferenced) != 0) {
            moduleUnit.addStaticDependency(className)
          }

          if ((flags & ReachabilityInfoInClass.FlagDynamicallyReferenced) != 0) {
            if (isNoModule)
              _errors ::= DynamicImportWithoutModuleSupport(from)
            else
              moduleUnit.addDynamicDependency(className)
          }
        }

        if (dataInClass.memberInfos != null) {
          dataInClass.memberInfos.foreach {
            case field: Infos.FieldReachable =>
              clazz.reachField(field)

            case Infos.StaticFieldReachable(fieldName, read, written) =>
              if (read)
                clazz._staticFieldsRead.update(fieldName, ())
              if (written)
                clazz._staticFieldsWritten.update(fieldName, ())

            case Infos.MethodReachable(methodName) =>
              clazz.callMethod(methodName)

            case Infos.MethodStaticallyReachable(namespace, methodName) =>
              clazz.callMethodStatically(namespace, methodName)

            case Infos.JSNativeMemberReachable(methodName) =>
              clazz.useJSNativeMember(methodName).foreach(addLoadSpec(moduleUnit, _))
          }
        }
      }
    }

    val globalFlags = data.globalFlags

    if (globalFlags != 0) {
      if ((globalFlags & ReachabilityInfo.FlagAccessedClassClass) != 0) {
        /* java.lang.Class is only ever instantiated in the CoreJSLib.
         * Therefore, make java.lang.Object depend on it instead of the caller itself.
         */
        objectClassInfo.addStaticDependency(ClassClass)
        lookupClass(ClassClass) { clazz =>
          clazz.instantiated()
          clazz.callMethodStatically(MemberNamespace.Constructor, ObjectArgConstructorName)
        }
      }

      if ((globalFlags & ReachabilityInfo.FlagAccessedNewTarget) != 0 &&
          config.coreSpec.esFeatures.esVersion < ESVersion.ES2015) {
        _errors ::= NewTargetWithoutES2015Support(from)
      }

      if ((globalFlags & ReachabilityInfo.FlagAccessedImportMeta) != 0 &&
          config.coreSpec.moduleKind != ModuleKind.ESModule) {
        _errors ::= ImportMetaWithoutESModule(from)
      }

      if ((globalFlags & ReachabilityInfo.FlagUsedExponentOperator) != 0 &&
          config.coreSpec.esFeatures.esVersion < ESVersion.ES2016) {
        _errors ::= ExponentOperatorWithoutES2016Support(from)
      }
    }
  }

  @tailrec
  private def addLoadSpec(moduleUnit: ModuleUnit,
      jsNativeLoadSpec: JSNativeLoadSpec): Unit = {
    jsNativeLoadSpec match {
      case _: JSNativeLoadSpec.Global =>

      case JSNativeLoadSpec.Import(module, _) =>
        moduleUnit.addExternalDependency(module)

      case JSNativeLoadSpec.ImportWithGlobalFallback(importSpec, _) =>
        if (!isNoModule)
          addLoadSpec(moduleUnit, importSpec)
    }
  }

  private def createMissingClassInfo(className: ClassName): Infos.ClassInfo = {
    val superClass =
      if (className == ObjectClass) None
      else Some(ObjectClass)

    val methods = Array.tabulate[Map[MethodName, Infos.MethodInfo]](MemberNamespace.Count) { nsOrdinal =>
      if (nsOrdinal == MemberNamespace.Constructor.ordinal)
        Map(NoArgConstructorName -> makeSyntheticMethodInfo())
      else
        Map.empty
    }

    new Infos.ClassInfo(className, ClassKind.Class,
        superClass = superClass, interfaces = Nil, jsNativeLoadSpec = None,
        referencedFieldClasses = Map.empty, methods = methods,
        jsNativeMembers = Map.empty, jsMethodProps = Nil, topLevelExports = Nil)
  }

  private def makeSyntheticMethodInfo(
      methodsCalled: List[(ClassName, MethodName)] = Nil,
      methodsCalledStatically: List[(ClassName, NamespacedMethodName)] = Nil
  ): Infos.MethodInfo = {
    val reachabilityInfoBuilder = new Infos.ReachabilityInfoBuilder(ir.Version.Unversioned)

    for ((className, methodName) <- methodsCalled)
      reachabilityInfoBuilder.addMethodCalled(className, methodName)
    for ((className, methodName) <- methodsCalledStatically)
      reachabilityInfoBuilder.addMethodCalledStatically(className, methodName)
    Infos.MethodInfo(isAbstract = false, reachabilityInfoBuilder.result())
  }

}

private object AnalyzerRun {
  private val getSuperclassMethodName =
    MethodName("getSuperclass", Nil, ClassRef(ClassClass))

  private class WorkTracker(implicit ec: ExecutionContext) {
    /** The number of tasks that have started but not completed, `+ 1` until
     *  `allowComplete()` gets called.
     */
    private val pending = new AtomicInteger(1)
    private val failures = new AtomicReference[List[Throwable]](Nil)
    private val promise = Promise[Unit]()

    def track(fut: Future[Unit]): Unit = {
      pending.incrementAndGet()

      fut.onComplete { result =>
        result match {
          case Success(_) => ()
          case Failure(t) => addFailure(t)
        }
        decrementPending()
      }
    }

    @tailrec
    private def addFailure(t: Throwable): Unit = {
      val prev = failures.get()
      if (!failures.compareAndSet(prev, t :: prev))
        addFailure(t)
    }

    private def decrementPending(): Unit = {
      /* When `pending` reaches 0, we are sure that all started tasks have
       * completed, and that `allowComplete()` was called. Therefore, no
       * further task can be concurrently added, and we are done.
       */
      if (pending.decrementAndGet() == 0)
        complete()
    }

    private def complete(): Unit = {
      failures.get() match {
        case Nil =>
          promise.success(())
        case firstFailure :: moreFailures =>
          for (t <- moreFailures)
            firstFailure.addSuppressed(t)
          promise.failure(firstFailure)
      }
    }

    /** Signals that no new top-level tasks will be started, and that it is
     *  therefore OK to complete the tracker once all ongoing tasks have finished.
     *
     *  `allowComplete()` must not be called more than once.
     */
    def allowComplete(): Future[Unit] = {
      decrementPending()
      promise.future
    }
  }

  private final class GrowingList[A] {
    private val list = new AtomicReference[List[A]](Nil)
    def ::=(item: A): Unit = list.updateAndGet(item :: _)
    def get(): List[A] = list.get()
    def addIfNil(item: A): Boolean = list.compareAndSet(Nil, item :: Nil)
    def clear(): Unit = list.set(Nil)
  }
}
