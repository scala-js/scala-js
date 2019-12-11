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

package org.scalajs.linker.frontend

import scala.concurrent._

import scala.collection.mutable

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._

import org.scalajs.logging._

import org.scalajs.linker._
import org.scalajs.linker.standard._
import org.scalajs.linker.analyzer._
import org.scalajs.linker.CollectionsCompat.MutableMapCompatOps

/** Does a dead code elimination pass on a [[standard.LinkingUnit LinkingUnit]].
 */
final class Refiner(config: CommonPhaseConfig) {
  import Refiner._

  private val inputProvider = new InputProvider

  def refine(unit: LinkingUnit, symbolRequirements: SymbolRequirement,
      logger: Logger)(implicit ec: ExecutionContext): Future[LinkingUnit] = {

    val linkedClassesByName =
      Map(unit.classDefs.map(c => c.className -> c): _*)
    inputProvider.update(linkedClassesByName)

    val analysis = logger.timeFuture("Refiner: Compute reachability") {
      analyze(symbolRequirements, logger)
    }

    for {
      analysis <- analysis
    } yield {
      val result = logger.time("Refiner: Assemble LinkedClasses") {
        val linkedClassDefs = for {
          info <- analysis.classInfos.values
        } yield {
          refineClassDef(linkedClassesByName(info.className), info)
        }

        new LinkingUnit(unit.coreSpec, linkedClassDefs.toList)
      }

      inputProvider.cleanAfterRun()

      result
    }
  }

  private def analyze(symbolRequirements: SymbolRequirement, logger: Logger)(
      implicit ec: ExecutionContext): Future[Analysis] = {
    for {
      analysis <- Analyzer.computeReachability(config, symbolRequirements,
          allowAddingSyntheticMethods = false,
          checkAbstractReachability = false, inputProvider)
    } yield {
      /* There must not be linking errors at this point. If there are, it is a
       * bug in the optimizer.
       */
      if (analysis.errors.isEmpty) {
        analysis
      } else {
        analysis.errors.foreach(Analysis.logError(_, logger, Level.Error))
        throw new AssertionError(
            "There were linking errors after the optimizer has run. " +
            "This is a bug, please report it. " +
            "You can work around the bug by disabling the optimizer. " +
            "In the sbt plugin, this can be done with " +
            "`scalaJSLinkerConfig ~= { _.withOptimizer(false) }`.")
      }
    }
  }

  private def refineClassDef(classDef: LinkedClass,
      info: Analysis.ClassInfo): LinkedClass = {

    val fields =
      if (info.isAnySubclassInstantiated) classDef.fields
      else Nil

    val methods = classDef.methods.filter { m =>
      val methodDef = m.value
      info.methodInfos(methodDef.flags.namespace)(methodDef.methodName)
        .isReachable
    }

    val kind =
      if (info.isModuleAccessed) classDef.kind
      else classDef.kind.withoutModuleAccessor

    classDef.refined(
        kind = kind,
        fields = fields,
        methods = methods,
        hasInstances = info.isAnySubclassInstantiated,
        hasInstanceTests = info.areInstanceTestsUsed,
        hasRuntimeTypeInfo = info.isDataAccessed)
  }

}

private object Refiner {
  private class InputProvider extends Analyzer.InputProvider {
    private var linkedClassesByName: Map[ClassName, LinkedClass] = _
    private val cache = mutable.Map.empty[ClassName, LinkedClassInfoCache]

    def update(linkedClassesByName: Map[ClassName, LinkedClass]): Unit = {
      this.linkedClassesByName = linkedClassesByName
    }

    def classesWithEntryPoints(): Iterable[ClassName] = {
      (for {
        linkedClass <- linkedClassesByName.valuesIterator
        if linkedClass.hasEntryPoint
      } yield {
        linkedClass.className
      }).toList
    }

    def loadInfo(className: ClassName)(implicit ec: ExecutionContext): Option[Future[Infos.ClassInfo]] =
      getCache(className).map(_.loadInfo(linkedClassesByName(className)))

    private def getCache(className: ClassName): Option[LinkedClassInfoCache] = {
      cache.get(className).orElse {
        if (linkedClassesByName.contains(className)) {
          val fileCache = new LinkedClassInfoCache
          cache += className -> fileCache
          Some(fileCache)
        } else {
          None
        }
      }
    }

    def cleanAfterRun(): Unit = {
      linkedClassesByName = null
      cache.filterInPlace((_, linkedClassCache) => linkedClassCache.cleanAfterRun())
    }
  }

  private class LinkedClassInfoCache {
    private var cacheUsed: Boolean = false
    private val methodsInfoCaches = LinkedMethodDefsInfosCache()
    private val exportedMembersInfoCaches = LinkedJSMethodPropDefsInfosCache()
    private var info: Infos.ClassInfo = _

    def loadInfo(linkedClass: LinkedClass)(implicit ec: ExecutionContext): Future[Infos.ClassInfo] = Future {
      update(linkedClass)
      info
    }

    private def update(linkedClass: LinkedClass): Unit = synchronized {
      if (!cacheUsed) {
        cacheUsed = true

        val builder = new Infos.ClassInfoBuilder(linkedClass.className)
          .setKind(linkedClass.kind)
          .setSuperClass(linkedClass.superClass.map(_.name))
          .addInterfaces(linkedClass.interfaces.map(_.name))

        for (field <- linkedClass.fields)
          builder.maybeAddReferencedFieldClass(field.ftpe)
        for (linkedMethod <- linkedClass.methods)
          builder.addMethod(methodsInfoCaches.getInfo(linkedMethod))
        for (info <- exportedMembersInfoCaches.getInfos(linkedClass.exportedMembers))
          builder.addExportedMember(info)

        if (linkedClass.topLevelExports.nonEmpty) {
          /* We do not cache top-level exports, because they're quite rare,
           * and usually quite small when they exist.
           */
          builder.setIsExported(true)

          val optInfo = Infos.generateTopLevelExportsInfo(
              linkedClass.className, linkedClass.topLevelExports.map(_.value))
          optInfo.foreach(builder.addTopLevelExportedMember(_))
        }

        info = builder.result()
      }
    }

    /** Returns true if the cache has been used and should be kept. */
    def cleanAfterRun(): Boolean = synchronized {
      val result = cacheUsed
      cacheUsed = false
      if (result) {
        // No point in cleaning the inner caches if the whole class disappears
        methodsInfoCaches.cleanAfterRun()
        exportedMembersInfoCaches.cleanAfterRun()
      }
      result
    }
  }

  private final class LinkedMethodDefsInfosCache private (
      val caches: Array[mutable.Map[MethodName, LinkedMethodDefInfoCache]])
      extends AnyVal {

    def getInfo(method: Versioned[MethodDef]): Infos.MethodInfo = {
      val methodDef = method.value
      val cache = caches(methodDef.flags.namespace.ordinal)
        .getOrElseUpdate(methodDef.methodName, new LinkedMethodDefInfoCache)
      cache.getInfo(method)
    }

    def cleanAfterRun(): Unit = {
      caches.foreach(_.filterInPlace((_, cache) => cache.cleanAfterRun()))
    }
  }

  private object LinkedMethodDefsInfosCache {
    def apply(): LinkedMethodDefsInfosCache = {
      new LinkedMethodDefsInfosCache(
          Array.fill(MemberNamespace.Count)(mutable.Map.empty))
    }
  }

  /* For JS method and property definitions, we use their index in the list of
   * `linkedClass.exportedMembers` as their identity. We cannot use their name
   * because the name itself is a `Tree`.
   *
   * If there is a different number of exported members than in a previous run,
   * we always recompute everything. This is fine because, for any given class,
   * either all JS methods and properties are reachable, or none are. So we're
   * only missing opportunities for incrementality in the case where JS members
   * are added or removed in the original .sjsir, which is not a big deal.
   */
  private final class LinkedJSMethodPropDefsInfosCache private (
      private var caches: Array[LinkedJSMethodPropDefInfoCache]) {

    def getInfos(members: List[Versioned[JSMethodPropDef]]): List[Infos.ReachabilityInfo] = {
      if (members.isEmpty) {
        caches = null
        Nil
      } else {
        val membersSize = members.size
        if (caches == null || membersSize != caches.size)
          caches = Array.fill(membersSize)(new LinkedJSMethodPropDefInfoCache)

        for ((member, i) <- members.zipWithIndex) yield {
          caches(i).getInfo(member)
        }
      }
    }

    // Just for consistency of API wrt. LinkedMethodDefsInfosCache
    def cleanAfterRun(): Unit = ()
  }

  private object LinkedJSMethodPropDefsInfosCache {
    def apply(): LinkedJSMethodPropDefsInfosCache =
      new LinkedJSMethodPropDefsInfosCache(null)
  }

  private abstract class AbstractLinkedMemberInfoCache[Def <: MemberDef, Info] {
    private var cacheUsed: Boolean = false
    private var lastVersion: Option[String] = None
    private var info: Info = _

    final def getInfo(member: Versioned[Def]): Info = {
      update(member)
      info
    }

    private final def update(member: Versioned[Def]): Unit = {
      if (!cacheUsed) {
        cacheUsed = true
        val newVersion = member.version
        if (!versionsMatch(newVersion, lastVersion)) {
          info = computeInfo(member.value)
          lastVersion = newVersion
        }
      }
    }

    protected def computeInfo(member: Def): Info

    /** Returns true if the cache has been used and should be kept. */
    final def cleanAfterRun(): Boolean = {
      val result = cacheUsed
      cacheUsed = false
      result
    }
  }

  private final class LinkedMethodDefInfoCache
      extends AbstractLinkedMemberInfoCache[MethodDef, Infos.MethodInfo] {

    protected  def computeInfo(member: MethodDef): Infos.MethodInfo =
      Infos.generateMethodInfo(member)
  }

  private final class LinkedJSMethodPropDefInfoCache
      extends AbstractLinkedMemberInfoCache[JSMethodPropDef, Infos.ReachabilityInfo] {

    protected  def computeInfo(member: JSMethodPropDef): Infos.ReachabilityInfo = {
      member match {
        case methodDef: JSMethodDef =>
          Infos.generateJSMethodInfo(methodDef)
        case propertyDef: JSPropertyDef =>
          Infos.generateJSPropertyInfo(propertyDef)
      }
    }
  }

  private def versionsMatch(a: Option[String], b: Option[String]): Boolean =
    a.isDefined && b.isDefined && a.get == b.get
}
