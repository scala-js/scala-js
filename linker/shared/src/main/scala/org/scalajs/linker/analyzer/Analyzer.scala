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

import scala.util.{Success, Failure}

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import org.scalajs.ir
import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees.{MemberNamespace, JSNativeLoadSpec}
import org.scalajs.ir.Types.ClassRef

import org.scalajs.linker._
import org.scalajs.linker.interface.{ESVersion, ModuleKind, ModuleInitializer}
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID

import Analysis._
import Infos.{NamespacedMethodName, ReachabilityInfo, ReachabilityInfoInClass}

private final class Analyzer(config: CommonPhaseConfig,
    moduleInitializers: Seq[ModuleInitializer],
    symbolRequirements: SymbolRequirement,
    allowAddingSyntheticMethods: Boolean,
    checkAbstractReachability: Boolean,
    inputProvider: Analyzer.InputProvider,
    ec: ExecutionContext)
    extends Analysis {

  import Analyzer._

  private val isNoModule = config.coreSpec.moduleKind == ModuleKind.NoModule

  private var objectClassInfo: ClassInfo = _
  private[this] val _classInfos = mutable.Map.empty[ClassName, ClassLoadingState]

  private[this] val _errors = mutable.Buffer.empty[Error]

  private val workQueue = new WorkQueue(ec)

  private val fromAnalyzer = FromCore("analyzer")

  private[this] var _loadedClassInfos: scala.collection.Map[ClassName, ClassInfo] = _

  def classInfos: scala.collection.Map[ClassName, Analysis.ClassInfo] =
    _loadedClassInfos

  private[this] val _topLevelExportInfos = mutable.Map.empty[(ModuleID, String), TopLevelExportInfo]

  def topLevelExportInfos: scala.collection.Map[(ModuleID, String), Analysis.TopLevelExportInfo] =
    _topLevelExportInfos

  def errors: scala.collection.Seq[Error] = _errors

  def computeReachability(): Future[Unit] = {
    require(_classInfos.isEmpty, "Cannot run the same Analyzer multiple times")

    loadObjectClass(() => loadEverything())

    workQueue.join().map(_ => postLoad())(ec)
  }

  private def loadObjectClass(onSuccess: () => Unit): Unit = {
    implicit val from = fromAnalyzer

    /* Load the java.lang.Object class, and validate it
     * If it is missing or invalid, we're in deep trouble, and cannot continue.
     */
    inputProvider.loadInfo(ObjectClass)(ec) match {
      case None =>
        _errors += MissingJavaLangObjectClass(fromAnalyzer)

      case Some(future) =>
        workQueue.enqueue(future) { data =>
          objectClassInfo = new ClassInfo(data,
              unvalidatedSuperClass = None,
              unvalidatedInterfaces = Nil, nonExistent = false)

          objectClassInfo.link()
          onSuccess()
        }
    }
  }

  private def loadEverything(): Unit = {
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
        objectClassInfo.staticDependencies += clazz.className
        clazz.instantiated()
      }
    }

    // External symbol requirements.
    reachSymbolRequirement(symbolRequirements)

    // Reach entry points
    for (className <- inputProvider.classesWithEntryPoints())
      lookupClass(className)(_.reachEntryPoints())

    // Reach module initializers.
    reachInitializers(moduleInitializers)
  }

  private def postLoad(): Unit = {
    if (isNoModule) {
      // Check there is only a single module.
      val publicModuleIDs = (
         topLevelExportInfos.keys.map(_._1).toList ++
         moduleInitializers.map(i => ModuleID(i.moduleID))
      ).distinct

      if (publicModuleIDs.size > 1)
        _errors += MultiplePublicModulesWithoutModuleSupport(publicModuleIDs)
    }

    // Assemble loaded infos.
    val infos = _classInfos.collect { case (k, i: ClassInfo) => (k, i) }

    assert(_errors.nonEmpty || infos.size == _classInfos.size,
        "unloaded classes in post load phase")

    _loadedClassInfos = infos

    // Reach additional data, based on reflection methods used
    reachDataThroughReflection(infos)
  }

  private def reachSymbolRequirement(requirement: SymbolRequirement,
      optional: Boolean = false): Unit = {

    /* We use j.l.Object as representation of the core infrastructure.
     * As such, everything depends on j.l.Object and j.l.Object depends on all
     * symbol requirements.
     */

    def withClass(className: ClassName)(onSuccess: ClassInfo => Unit)(
        implicit from: From): Unit = {
      lookupClass(className, ignoreMissing = optional)(onSuccess)
    }

    def withMethod(className: ClassName, methodName: MethodName)(
        onSuccess: ClassInfo => Unit)(
        implicit from: From): Unit = {
      withClass(className) { clazz =>
        val doReach = !optional || clazz.tryLookupMethod(methodName).isDefined
        if (doReach)
          onSuccess(clazz)
      }
    }

    import SymbolRequirement.Nodes._

    requirement match {
      case AccessModule(origin, moduleName) =>
        implicit val from = FromCore(origin)
        withClass(moduleName) { clazz =>
          objectClassInfo.staticDependencies += clazz.className
          clazz.accessModule()
        }

      case InstantiateClass(origin, className, constructor) =>
        implicit val from = FromCore(origin)
        withMethod(className, constructor) { clazz =>
          objectClassInfo.staticDependencies += clazz.className
          clazz.instantiated()
          clazz.callMethodStatically(MemberNamespace.Constructor, constructor)
        }

      case InstanceTests(origin, className) =>
        implicit val from = FromCore(origin)
        withClass(className){ clazz =>
          objectClassInfo.staticDependencies += clazz.className
          clazz.useInstanceTests()
        }

      case ClassData(origin, className) =>
        implicit val from = FromCore(origin)
        withClass(className) { clazz =>
          objectClassInfo.staticDependencies += clazz.className
          clazz.accessData()
        }

      case CallMethod(origin, className, methodName, statically) =>
        implicit val from = FromCore(origin)
        withMethod(className, methodName) { clazz =>
          if (statically) {
            objectClassInfo.staticDependencies += clazz.className
            clazz.callMethodStatically(MemberNamespace.Public, methodName)
          } else {
            clazz.callMethod(methodName)
          }
        }

      case CallStaticMethod(origin, className, methodName) =>
        implicit val from = FromCore(origin)
        withMethod(className, methodName) { clazz =>
          objectClassInfo.staticDependencies += clazz.className
          clazz.callMethodStatically(MemberNamespace.PublicStatic, methodName)
        }

      case Optional(requirement) =>
        reachSymbolRequirement(requirement, optional = true)

      case Multiple(requirements) =>
        for (requirement <- requirements)
          reachSymbolRequirement(requirement, optional)

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
  private def reachDataThroughReflection(
      classInfos: scala.collection.Map[ClassName, ClassInfo]): Unit = {

    val classClassInfo = classInfos.get(ClassClass)

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
      for (classInfo <- classInfos.values.filter(_.isDataAccessed).toList) {
        @tailrec
        def loop(classInfo: ClassInfo): Unit = {
          classInfo.accessData()
          classInfo.superClass match {
            case Some(superClass) =>
              classInfo.staticDependencies += superClass.className
              loop(superClass)

            case None =>
          }
        }
        loop(classInfo)
      }
    }
  }

  private def lookupClass(className: ClassName,
      ignoreMissing: Boolean = false)(
      onSuccess: ClassInfo => Unit)(implicit from: From): Unit = {
    lookupClassForLinking(className, Set.empty) {
      case info: ClassInfo =>
        if (!info.nonExistent || !ignoreMissing) {
          info.link()
          onSuccess(info)
        }

      case CycleInfo(cycle, _) =>
        _errors += CycleInInheritanceChain(cycle, fromAnalyzer)
    }
  }

  private def lookupClassForLinking(className: ClassName,
      knownDescendants: Set[LoadingClass] = Set.empty)(
      onSuccess: LoadingResult => Unit): Unit = {

    _classInfos.get(className) match {
      case None =>
        val loading = new LoadingClass(className)
        loading.requestLink(knownDescendants)(onSuccess)

      case Some(loading: LoadingClass) =>
        loading.requestLink(knownDescendants)(onSuccess)

      case Some(info: ClassInfo) =>
        onSuccess(info)
    }
  }


  private sealed trait LoadingResult
  private sealed trait ClassLoadingState

  // sealed instead of final because of spurious unchecked warnings
  private sealed case class CycleInfo(cycle: List[ClassName],
      root: LoadingClass)
      extends LoadingResult

  private final class LoadingClass(className: ClassName)
      extends ClassLoadingState {

    private val promise = Promise[LoadingResult]()
    private var knownDescendants = Set[LoadingClass](this)

    _classInfos(className) = this

    inputProvider.loadInfo(className)(ec) match {
      case Some(future) =>
        workQueue.enqueue(future)(link(_, nonExistent = false))

      case None =>
        val data = createMissingClassInfo(className)
        link(data, nonExistent = true)
    }

    def requestLink(knownDescendants: Set[LoadingClass])(onSuccess: LoadingResult => Unit): Unit = {
      if (knownDescendants.contains(this)) {
        onSuccess(CycleInfo(Nil, this))
      } else {
        this.knownDescendants ++= knownDescendants
        workQueue.enqueue(promise.future)(onSuccess)
      }
    }

    private def link(data: Infos.ClassInfo, nonExistent: Boolean): Unit = {
      lookupAncestors(data.superClass.toList ++ data.interfaces) { classes =>
        val (superClass, interfaces) =
          if (data.superClass.isEmpty) (None, classes)
          else (Some(classes.head), classes.tail)

        val info = new ClassInfo(data, superClass, interfaces, nonExistent)

        implicit val from = FromClass(info)
        classes.foreach(_.link())

        promise.success(info)
      } { cycleInfo =>
        val newInfo = cycleInfo match {
          case CycleInfo(_, null) => cycleInfo

          case CycleInfo(c, root) if root == this =>
            CycleInfo(className :: c, null)

          case CycleInfo(c, root) =>
            CycleInfo(className :: c, root)
        }

        promise.success(newInfo)
      }
    }

    private def lookupAncestors(classNames: List[ClassName])(
        loaded: List[ClassInfo] => Unit)(cycle: CycleInfo => Unit): Unit = {
      classNames match {
        case first :: rest =>
          lookupClassForLinking(first, knownDescendants) {
            case c: CycleInfo => cycle(c)

            case ifirst: ClassInfo =>
              lookupAncestors(rest)(irest => loaded(ifirst :: irest))(cycle)
          }
        case Nil =>
          loaded(Nil)
      }
    }
  }

  private class ClassInfo(
      val data: Infos.ClassInfo,
      unvalidatedSuperClass: Option[ClassInfo],
      unvalidatedInterfaces: List[ClassInfo],
      val nonExistent: Boolean)
      extends Analysis.ClassInfo with ClassLoadingState with LoadingResult {

    var linkedFrom: List[From] = Nil

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

    // Note: j.l.Object is special and is validated upfront

    val superClass: Option[ClassInfo] =
      if (className == ObjectClass) unvalidatedSuperClass
      else validateSuperClass(unvalidatedSuperClass)

    val interfaces: List[ClassInfo] =
      if (className == ObjectClass) unvalidatedInterfaces
      else validateInterfaces(unvalidatedInterfaces)

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

    _classInfos(className) = this

    def link()(implicit from: From): Unit = {
      if (nonExistent)
        _errors += MissingClass(this, from)

      linkedFrom ::= from
    }

    private[this] def validateSuperClass(superClass: Option[ClassInfo]): Option[ClassInfo] = {
      def from = FromClass(this)

      kind match {
        case ClassKind.Class | ClassKind.ModuleClass | ClassKind.HijackedClass =>
          val superCl = superClass.get // checked by ClassDef checker.
          if (superCl.kind != ClassKind.Class) {
            _errors += InvalidSuperClass(superCl, this, from)
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
              _errors += InvalidSuperClass(superCl, this, from)
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
              _errors += InvalidSuperClass(superCl, this, from)
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
                _errors += InvalidSuperClass(superCl, this, from)
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
          _errors += InvalidImplementedInterface(superIntf, this, from)
          false
        } else {
          true
        }
      }
    }

    var isInstantiated: Boolean = false
    var isAnySubclassInstantiated: Boolean = false
    var isModuleAccessed: Boolean = false
    var areInstanceTestsUsed: Boolean = false
    var isDataAccessed: Boolean = false

    val fieldsRead: mutable.Set[FieldName] = mutable.Set.empty
    val fieldsWritten: mutable.Set[FieldName] = mutable.Set.empty
    val staticFieldsRead: mutable.Set[FieldName] = mutable.Set.empty
    val staticFieldsWritten: mutable.Set[FieldName] = mutable.Set.empty

    val jsNativeMembersUsed: mutable.Set[MethodName] = mutable.Set.empty

    val jsNativeLoadSpec: Option[JSNativeLoadSpec] = data.jsNativeLoadSpec

    /* j.l.Object represents the core infrastructure. As such, everything
     * depends on it unconditionally.
     */
    val staticDependencies: mutable.Set[ClassName] =
      if (className == ObjectClass) mutable.Set.empty
      else mutable.Set(ObjectClass)

    val externalDependencies: mutable.Set[String] = mutable.Set.empty

    val dynamicDependencies: mutable.Set[ClassName] = mutable.Set.empty

    var instantiatedFrom: List[From] = Nil

    val dispatchCalledFrom: mutable.Map[MethodName, List[From]] = mutable.Map.empty

    /** List of all instantiated (Scala) subclasses of this Scala class/trait.
     *  For JS types, this always remains empty.
     */
    var instantiatedSubclasses: List[ClassInfo] = Nil
    var methodsCalledLog: List[MethodName] = Nil

    private val nsMethodInfos = {
      val nsMethodInfos = Array.fill(MemberNamespace.Count) {
        mutable.Map.empty[MethodName, MethodInfo]
      }
      for (methodData <- data.methods) {
        // TODO It would be good to report duplicates as errors at this point
        val relevantMap = nsMethodInfos(methodData.namespace.ordinal)
        relevantMap(methodData.methodName) = new MethodInfo(this, methodData)
      }
      nsMethodInfos
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
        if !m.isDefaultBridge
      } yield {
        m
      }

      if (candidatesIterator.isEmpty)
        createNonExistentMethod(methodName)
      else
        candidatesIterator.next()
    }

    def lookupMethod(methodName: MethodName): MethodInfo = {
      tryLookupMethod(methodName).getOrElse {
        createNonExistentMethod(methodName)
      }
    }

    private def createNonExistentMethod(methodName: MethodName): MethodInfo = {
      val syntheticData = makeSyntheticMethodInfo(methodName)
      val m = new MethodInfo(this, syntheticData)
      m.nonExistent = true
      publicMethodInfos += methodName -> m
      m
    }

    def tryLookupMethod(methodName: MethodName): Option[MethodInfo] = {
      assert(isScalaClass || isInterface,
          s"Cannot call lookupMethod($methodName) on non Scala class $this")

      @tailrec
      def tryLookupInherited(ancestorInfo: ClassInfo): Option[MethodInfo] = {
        ancestorInfo.publicMethodInfos.get(methodName) match {
          case Some(m) if !m.isAbstract =>
            Some(m)
          case _ =>
            ancestorInfo.superClass match {
              case Some(superClass) => tryLookupInherited(superClass)
              case None             => None
            }
        }
      }
      val existing =
        if (isScalaClass) tryLookupInherited(this)
        else publicMethodInfos.get(methodName).filter(!_.isAbstract)

      if (!allowAddingSyntheticMethods) {
        existing
      } else if (existing.exists(m => !m.isDefaultBridge || m.owner == this)) {
        /* If we found a non-bridge, it must be the right target.
         * If we found a bridge directly in this class/interface, it must also
         * be the right target.
         */
        existing
      } else {
        // Try and find the target of a possible default bridge
        findDefaultTarget(methodName).fold {
          assert(existing.isEmpty)
          existing
        } { defaultTarget =>
          if (existing.exists(_.defaultBridgeTarget == defaultTarget.owner.className)) {
            /* If we found an existing bridge targeting the right method, we
             * can reuse it.
             * We also get here with None when there is no target whatsoever.
             */
            existing
          } else {
            // Otherwise, create a new default bridge
            Some(createDefaultBridge(defaultTarget))
          }
        }
      }
    }

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
        if !m.isAbstract && !m.isDefaultBridge
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
        _errors += ConflictingDefaultMethods(notShadowed, fromAnalyzer)
      }

      notShadowed.headOption
    }

    private def createDefaultBridge(target: MethodInfo): MethodInfo = {
      val methodName = target.methodName
      val targetOwner = target.owner

      val syntheticInfo = makeSyntheticMethodInfo(
          methodName = methodName,
          methodsCalledStatically = List(
              targetOwner.className -> NamespacedMethodName(MemberNamespace.Public, methodName)))
      val m = new MethodInfo(this, syntheticInfo)
      m.syntheticKind = MethodSyntheticKind.DefaultBridge(
          targetOwner.className)
      publicMethodInfos += methodName -> m
      m
    }

    def tryLookupReflProxyMethod(proxyName: MethodName)(
        onSuccess: MethodInfo => Unit)(implicit from: From): Unit = {
      if (!allowAddingSyntheticMethods) {
        tryLookupMethod(proxyName).foreach(onSuccess)
      } else {
        publicMethodInfos.get(proxyName).fold {
          workQueue.enqueue(findReflectiveTarget(proxyName)) { maybeTarget =>
            maybeTarget.foreach { reflectiveTarget =>
              val proxy = createReflProxy(proxyName, reflectiveTarget.methodName)
              onSuccess(proxy)
            }
          }
        } (onSuccess)
      }
    }

    private def findReflectiveTarget(proxyName: MethodName)(
        implicit from: From): Future[Option[MethodInfo]] = {
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

      val candidates = ancestorsInReflectiveTargetOrder.iterator.map(_.findProxyMatch(proxyName))

      locally {
        implicit val iec = ec

        /* Manual version of
         * Future.sequence(candidates).map(_.collectFirst { case Some(m) => m })
         *
         * We use a manual version because `Future.sequence` uses zipWith, not
         * flatMap, and therefore does not wait for one future to complete
         * before continuing on to the next one. This is generally good
         * behavior, but it kills our use case, as it bypasses the
         * short-circuiting behavior of `Iterator`s, resulting in *a lot* of
         * useless computations!
         */
        def loop(): Future[Option[MethodInfo]] = {
          if (candidates.isEmpty) {
            Future.successful(None)
          } else {
            val candidate = candidates.next()
            candidate.flatMap { optResult =>
              if (optResult.isDefined)
                Future.successful(optResult)
              else
                loop()
            }
          }
        }

        loop()
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

    private def findProxyMatch(proxyName: MethodName)(
        implicit from: From): Future[Option[MethodInfo]] = {
      val candidates = findProxyCandidates(proxyName)

      // Fast paths for 0 and 1 candidates
      candidates match {
        case Nil                  => Future.successful(None)
        case onlyCandidate :: Nil => Future.successful(Some(onlyCandidate))
        case _                    => computeMostSpecificProxyMatch(candidates)
      }
    }

    private def findProxyCandidates(proxyName: MethodName): List[MethodInfo] = {
      // This is a manual version of .filter(...).toList, for speed
      var result: List[MethodInfo] = Nil
      val iter = publicMethodInfos.valuesIterator
      while (iter.hasNext) {
        val m = iter.next()
        val include = {
          // TODO In theory we should filter out protected methods
          !m.isReflectiveProxy && !m.isDefaultBridge && !m.isAbstract &&
          reflProxyMatches(m.methodName, proxyName)
        }
        if (include)
          result ::= m
      }
      result
    }

    private def computeMostSpecificProxyMatch(candidates: List[MethodInfo])(
        implicit from: From): Future[Option[MethodInfo]] = {

      /* From the JavaDoc of java.lang.Class.getMethod:
       *
       *   If more than one [candidate] method is found in C, and one of these
       *   methods has a return type that is more specific than any of the
       *   others, that method is reflected; otherwise one of the methods is
       *   chosen arbitrarily.
       */

      val resultTypes = candidates.map(c => c.methodName.resultTypeRef)

      // We must not use Future.traverse since otherwise we might run things on
      // the non-main thread.
      val specificityChecks = resultTypes.map { x =>
        for (y <- resultTypes if x != y)
          yield isMoreSpecific(y, x)
      }

      // Starting here, we just do data juggling, so it can run on any thread.
      locally {
        implicit val iec = ec

        val hasMoreSpecific = Future.traverse(specificityChecks)(
            checks => Future.sequence(checks).map(_.contains(true)))

        hasMoreSpecific.map { hms =>
          val targets = candidates.zip(hms).filterNot(_._2).map(_._1)

          /* This last step (chosen arbitrarily) causes some soundness issues of
           * the implementation of reflective calls. This is bug-compatible with
           * Scala/JVM.
           */
          targets.headOption
        }
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

      val syntheticInfo = makeSyntheticMethodInfo(
          methodName = proxyName,
          methodsCalled = List(this.className -> targetName))
      val m = new MethodInfo(this, syntheticInfo)
      m.syntheticKind = MethodSyntheticKind.ReflectiveProxy(targetName)
      publicMethodInfos += proxyName -> m
      m
    }

    def lookupStaticLikeMethod(namespace: MemberNamespace,
        methodName: MethodName): MethodInfo = {
      tryLookupStaticLikeMethod(namespace, methodName).getOrElse {
        val syntheticData = makeSyntheticMethodInfo(methodName, namespace)
        val m = new MethodInfo(this, syntheticData)
        m.nonExistent = true
        methodInfos(namespace)(methodName) = m
        m
      }
    }

    def tryLookupStaticLikeMethod(namespace: MemberNamespace,
        methodName: MethodName): Option[MethodInfo] = {
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

        _topLevelExportInfos.get(key).fold[Unit] {
          _topLevelExportInfos.put(key, info)
        } { other =>
          _errors += ConflictingTopLevelExport(tle.moduleID, tle.exportName, List(info, other))
        }
      }
    }

    def accessModule()(implicit from: From): Unit = {
      if (!isAnyModuleClass) {
        _errors += NotAModule(this, from)
      } else if (!isModuleAccessed) {
        isModuleAccessed = true

        instantiated()
        if (isScalaClass)
          callMethodStatically(MemberNamespace.Constructor, NoArgConstructorName)
      }
    }

    def instantiated()(implicit from: From): Unit = {
      instantiatedFrom ::= from

      /* TODO? When the second line is false, shouldn't this be a linking error
       * instead?
       */
      if (!isInstantiated &&
          (isScalaClass || isJSClass || isNativeJSClass)) {
        isInstantiated = true

        // TODO: Why is this not in subclassInstantiated()?
        referenceFieldClasses(fieldsRead ++ fieldsWritten)

        if (isScalaClass) {
          accessData()

          /* First mark the ancestors as subclassInstantiated() and fetch the
           * methodsCalledLog, for all ancestors. Only then perform the
           * resolved calls for all the logs. This order is important because,
           * during the resolved calls, new methods could be called and added
           * to the log; they will already see the new subclasses so we should
           * *not* see them in the logs, lest we perform some work twice.
           */

          val allMethodsCalledLogs = for (ancestor <- ancestors) yield {
            ancestor.subclassInstantiated()
            ancestor.instantiatedSubclasses ::= this
            ancestor -> ancestor.methodsCalledLog
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
            followReachabilityInfo(reachabilityInfo, staticDependencies,
                externalDependencies, dynamicDependencies)(FromExports)
        }
      }
    }

    private def subclassInstantiated()(implicit from: From): Unit = {
      instantiatedFrom ::= from
      if (!isAnySubclassInstantiated && (isScalaClass || isJSType)) {
        isAnySubclassInstantiated = true

        if (!isNativeJSClass) {
          for (clazz <- superClass) {
            if (clazz.isNativeJSClass)
              clazz.jsNativeLoadSpec.foreach(addLoadSpec(externalDependencies, _))
            else
              staticDependencies += clazz.className
          }
        }

        // Reach exported members
        if (!isJSClass) {
          for (reachabilityInfo <- data.jsMethodProps)
            followReachabilityInfo(reachabilityInfo, staticDependencies,
                externalDependencies, dynamicDependencies)(FromExports)
        }
      }
    }

    def useInstanceTests()(implicit from: From): Unit = {
      if (!areInstanceTestsUsed)
        areInstanceTestsUsed = true
    }

    def accessData()(implicit from: From): Unit = {
      if (!isDataAccessed) {
        isDataAccessed = true

        // #4548 The `isInstance` function will refer to the class value
        if (kind == ClassKind.NativeJSClass)
          jsNativeLoadSpec.foreach(addLoadSpec(externalDependencies, _))
      }
    }

    def callMethod(methodName: MethodName)(implicit from: From): Unit = {
      /* First add the call to the log, then fetch the instantiated subclasses,
       * then perform the resolved call. This order is important because,
       * during the resolved calls, new instantiated subclasses could be
       * detected, and those need to see the updated log, since the loop in
       * this method won't see them.
       */

      dispatchCalledFrom.get(methodName) match {
        case Some(froms) =>
          // Already called before; add the new from
          dispatchCalledFrom.update(methodName, from :: froms)

        case None =>
          // New call
          dispatchCalledFrom.update(methodName, from :: Nil)

          val fromDispatch = FromDispatch(this, methodName)

          methodsCalledLog ::= methodName
          val subclasses = instantiatedSubclasses
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
      }
    }

    private def callMethodResolved(methodName: MethodName)(
        implicit from: From): Unit = {
      if (methodName.isReflectiveProxy) {
        tryLookupReflProxyMethod(methodName)(_.reach(this))
      } else {
        lookupMethod(methodName).reach(this)
      }
    }

    def callMethodStatically(namespacedMethodName: NamespacedMethodName)(
        implicit from: From): Unit = {
      callMethodStatically(namespacedMethodName.namespace,
          namespacedMethodName.methodName)
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

    def readFields(names: List[FieldName])(implicit from: From): Unit = {
      fieldsRead ++= names
      if (isInstantiated)
        referenceFieldClasses(names)
    }

    def writeFields(names: List[FieldName])(implicit from: From): Unit = {
      fieldsWritten ++= names
      if (isInstantiated)
        referenceFieldClasses(names)
    }

    def useJSNativeMember(name: MethodName)(
        implicit from: From): Option[JSNativeLoadSpec] = {
      val maybeJSNativeLoadSpec = data.jsNativeMembers.get(name)
      if (jsNativeMembersUsed.add(name)) {
        maybeJSNativeLoadSpec match {
          case None =>
            _errors += MissingJSNativeMember(this, name, from)
          case Some(jsNativeLoadSpec) =>
            validateLoadSpec(jsNativeLoadSpec, Some(name))
        }
      }
      maybeJSNativeLoadSpec
    }

    private def referenceFieldClasses(fieldNames: Iterable[FieldName])(
        implicit from: From): Unit = {
      assert(isInstantiated)

      /* Reach referenced classes of non-static fields
       *
       * We do not need to add this to staticDependencies: The definition
       * site will not reference the classes in the final JS code.
       */
      for {
        fieldName <- fieldNames
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
            _errors += ImportWithoutModuleSupport(module, this, jsNativeMember, from)
          case _ =>
        }
      }
    }
  }

  private class MethodInfo(val owner: ClassInfo,
      data: Infos.MethodInfo) extends Analysis.MethodInfo {

    val methodName = data.methodName
    val namespace = data.namespace
    val isAbstract = data.isAbstract

    var isAbstractReachable: Boolean = false
    var isReachable: Boolean = false

    var calledFrom: List[From] = Nil
    var instantiatedSubclasses: List[ClassInfo] = Nil

    var nonExistent: Boolean = false

    var syntheticKind: MethodSyntheticKind = MethodSyntheticKind.None

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
      assert(!isAbstract,
          s"Trying to reach statically the abstract method $this")

      checkExistent()

      calledFrom ::= from
      if (!isReachable) {
        isAbstractReachable = true
        isReachable = true
        doReach()
      }
    }

    def reachAbstract()(implicit from: From): Unit = {
      assert(namespace == MemberNamespace.Public)

      if (!isAbstractReachable) {
        checkExistent()
        calledFrom ::= from
        isAbstractReachable = true
      }
    }

    def reach(inClass: ClassInfo)(implicit from: From): Unit = {
      assert(!namespace.isStatic,
          s"Trying to dynamically reach the static method $this")
      assert(!isAbstract,
          s"Trying to dynamically reach the abstract method $this")
      assert(owner.isAnyClass,
          s"Trying to dynamically reach the non-class method $this")
      assert(!namespace.isConstructor,
          s"Trying to dynamically reach the constructor $this")

      checkExistent()

      calledFrom ::= from
      instantiatedSubclasses ::= inClass

      if (!isReachable) {
        isAbstractReachable = true
        isReachable = true
        doReach()
      }
    }

    private def checkExistent()(implicit from: From) = {
      if (nonExistent)
        _errors += MissingMethod(this, from)
    }

    private[this] def doReach(): Unit = {
      followReachabilityInfo(data.reachabilityInfo, owner.staticDependencies,
          owner.externalDependencies, owner.dynamicDependencies)(FromMethod(this))
    }
  }

  private class TopLevelExportInfo(val owningClass: ClassName, data: Infos.TopLevelExportInfo)
      extends Analysis.TopLevelExportInfo {
    val moduleID: ModuleID = data.moduleID
    val exportName: String = data.exportName

    if (isNoModule && !ir.Trees.JSGlobalRef.isValidJSGlobalRefName(exportName)) {
      _errors += InvalidTopLevelExportInScript(this)
    }

    val staticDependencies: mutable.Set[ClassName] = mutable.Set.empty
    val externalDependencies: mutable.Set[String] = mutable.Set.empty

    def reach(): Unit = {
      val dynamicDependencies = mutable.Set.empty[ClassName]
      followReachabilityInfo(data.reachability, staticDependencies,
          externalDependencies, dynamicDependencies)(FromExports)
    }
  }

  private def followReachabilityInfo(data: ReachabilityInfo,
      staticDependencies: mutable.Set[ClassName],
      externalDependencies: mutable.Set[String],
      dynamicDependencies: mutable.Set[ClassName])(
      implicit from: From): Unit = {

    def addInstanceDependency(info: ClassInfo) = {
      info.jsNativeLoadSpec.foreach(addLoadSpec(externalDependencies, _))
      if (info.kind.isAnyNonNativeClass)
        staticDependencies += info.className
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
            staticDependencies += className
            clazz.useInstanceTests()
          }

          if ((flags & ReachabilityInfoInClass.FlagClassDataAccessed) != 0) {
            staticDependencies += className
            clazz.accessData()
          }

          if ((flags & ReachabilityInfoInClass.FlagStaticallyReferenced) != 0) {
            staticDependencies += className
          }
        }

        /* Since many of the lists below are likely to be empty, we always
         * test `!list.isEmpty` before calling `foreach` or any other
         * processing, avoiding closure allocations.
         */

        if (!dataInClass.fieldsRead.isEmpty) {
          clazz.readFields(dataInClass.fieldsRead)
        }

        if (!dataInClass.fieldsWritten.isEmpty) {
          clazz.writeFields(dataInClass.fieldsWritten)
        }

        if (!dataInClass.staticFieldsRead.isEmpty) {
          staticDependencies += className
          clazz.staticFieldsRead ++= dataInClass.staticFieldsRead
        }

        if (!dataInClass.staticFieldsWritten.isEmpty) {
          staticDependencies += className
          clazz.staticFieldsWritten ++= dataInClass.staticFieldsWritten
        }

        if (!dataInClass.methodsCalled.isEmpty) {
          // Do not add to staticDependencies: We call these on the object.
          for (methodName <- dataInClass.methodsCalled)
            clazz.callMethod(methodName)
        }

        if (!dataInClass.methodsCalledStatically.isEmpty) {
          staticDependencies += className
          for (methodName <- dataInClass.methodsCalledStatically)
            clazz.callMethodStatically(methodName)
        }

        if (!dataInClass.methodsCalledDynamicImport.isEmpty) {
          if (isNoModule) {
            _errors += DynamicImportWithoutModuleSupport(from)
          } else {
            dynamicDependencies += className
            // In terms of reachability, a dynamic import call is just a static call.
            for (methodName <- dataInClass.methodsCalledDynamicImport)
              clazz.callMethodStatically(methodName)
          }
        }

        if (!dataInClass.jsNativeMembersUsed.isEmpty) {
          for (member <- dataInClass.jsNativeMembersUsed)
            clazz.useJSNativeMember(member)
              .foreach(addLoadSpec(externalDependencies, _))
        }
      }
    }

    val globalFlags = data.globalFlags

    if (globalFlags != 0) {
      if ((globalFlags & ReachabilityInfo.FlagAccessedClassClass) != 0) {
        /* java.lang.Class is only ever instantiated in the CoreJSLib.
         * Therefore, make java.lang.Object depend on it instead of the caller itself.
         */
        objectClassInfo.staticDependencies += ClassClass
        lookupClass(ClassClass) { clazz =>
          clazz.instantiated()
          clazz.callMethodStatically(MemberNamespace.Constructor, ObjectArgConstructorName)
        }
      }

      if ((globalFlags & ReachabilityInfo.FlagAccessedNewTarget) != 0 &&
          config.coreSpec.esFeatures.esVersion < ESVersion.ES2015) {
        _errors += NewTargetWithoutES2015Support(from)
      }

      if ((globalFlags & ReachabilityInfo.FlagAccessedImportMeta) != 0 &&
          config.coreSpec.moduleKind != ModuleKind.ESModule) {
        _errors += ImportMetaWithoutESModule(from)
      }

      if ((globalFlags & ReachabilityInfo.FlagUsedExponentOperator) != 0 &&
          config.coreSpec.esFeatures.esVersion < ESVersion.ES2016) {
        _errors += ExponentOperatorWithoutES2016Support(from)
      }
    }
  }

  @tailrec
  private def addLoadSpec(externalDependencies: mutable.Set[String],
      jsNativeLoadSpec: JSNativeLoadSpec): Unit = {
    jsNativeLoadSpec match {
      case _: JSNativeLoadSpec.Global =>

      case JSNativeLoadSpec.Import(module, _) =>
        externalDependencies += module

      case JSNativeLoadSpec.ImportWithGlobalFallback(importSpec, _) =>
        if (!isNoModule)
          addLoadSpec(externalDependencies, importSpec)
    }
  }

  private def createMissingClassInfo(className: ClassName): Infos.ClassInfo = {
    new Infos.ClassInfoBuilder(className, ClassKind.Class,
        superClass = Some(ObjectClass), interfaces = Nil, jsNativeLoadSpec = None)
      .addMethod(makeSyntheticMethodInfo(NoArgConstructorName))
      .result()
  }

  private def makeSyntheticMethodInfo(
      methodName: MethodName,
      namespace: MemberNamespace = MemberNamespace.Public,
      methodsCalled: List[(ClassName, MethodName)] = Nil,
      methodsCalledStatically: List[(ClassName, NamespacedMethodName)] = Nil,
      instantiatedClasses: List[ClassName] = Nil
  ): Infos.MethodInfo = {
    val reachabilityInfoBuilder = new Infos.ReachabilityInfoBuilder()
    for ((className, methodName) <- methodsCalled)
      reachabilityInfoBuilder.addMethodCalled(className, methodName)
    for ((className, methodName) <- methodsCalledStatically)
      reachabilityInfoBuilder.addMethodCalledStatically(className, methodName)
    Infos.MethodInfo(methodName, namespace, isAbstract = false,
        reachabilityInfoBuilder.result())
  }

}

object Analyzer {
  private val getSuperclassMethodName =
    MethodName("getSuperclass", Nil, ClassRef(ClassClass))

  def computeReachability(config: CommonPhaseConfig,
      moduleInitializers: Seq[ModuleInitializer],
      symbolRequirements: SymbolRequirement,
      allowAddingSyntheticMethods: Boolean,
      checkAbstractReachability: Boolean,
      inputProvider: InputProvider)(implicit ec: ExecutionContext): Future[Analysis] = {
    val analyzer = new Analyzer(config, moduleInitializers, symbolRequirements,
        allowAddingSyntheticMethods, checkAbstractReachability, inputProvider, ec)
    analyzer.computeReachability().map(_ => analyzer)
  }

  trait InputProvider {
    def classesWithEntryPoints(): Iterable[ClassName]

    def loadInfo(className: ClassName)(
        implicit ec: ExecutionContext): Option[Future[Infos.ClassInfo]]
  }

  private class WorkQueue(ec: ExecutionContext) {
    private val queue = new ConcurrentLinkedQueue[() => Unit]()
    private val working = new AtomicBoolean(false)
    private val pending = new AtomicInteger(0)
    private val promise = Promise[Unit]()

    def enqueue[T](fut: Future[T])(onSuccess: T => Unit): Unit = {
      val got = pending.incrementAndGet()
      assert(got > 0)

      fut.onComplete {
        case Success(r) =>
          queue.add(() => onSuccess(r))
          tryDoWork()

        case Failure(t) =>
          promise.tryFailure(t)
      } (ec)
    }

    def join(): Future[Unit] = {
      tryDoWork()
      promise.future
    }

    @tailrec
    private def tryDoWork(): Unit = {
      if (!working.getAndSet(true)) {
        while (!queue.isEmpty) {
          try {
            val work = queue.poll()
            work()
          } catch {
            case t: Throwable => promise.tryFailure(t)
          }

          pending.decrementAndGet()
        }

        if (pending.compareAndSet(0, -1)) {
          assert(queue.isEmpty)
          promise.trySuccess(())
        }

        working.set(false)

        /* Another thread might have inserted work in the meantime but not yet
         * seen that we released the lock. Try and work steal again if this
         * happens.
         */
        if (!queue.isEmpty) tryDoWork()
      }
    }
  }
}
