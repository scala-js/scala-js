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

import org.scalajs.ir.Version
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._

import org.scalajs.logging._

import org.scalajs.linker._
import org.scalajs.linker.interface.ModuleInitializer
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID
import org.scalajs.linker.analyzer._
import org.scalajs.linker.CollectionsCompat.MutableMapCompatOps

/** Does a dead code elimination pass on a [[LinkingUnit]]. */
final class Refiner(config: CommonPhaseConfig) {
  import Refiner._

  private val inputProvider = new InputProvider

  def refine(classDefs: Seq[(ClassDef, Version)],
      moduleInitializers: List[ModuleInitializer],
      symbolRequirements: SymbolRequirement, logger: Logger)(
      implicit ec: ExecutionContext): Future[LinkingUnit] = {

    val linkedClassesByName = classDefs.map(c => c._1.className -> c._1).toMap
    inputProvider.update(linkedClassesByName)

    val analysis = logger.timeFuture("Refiner: Compute reachability") {
      analyze(moduleInitializers, symbolRequirements, logger)
    }

    for {
      analysis <- analysis
    } yield {
      val result = logger.time("Refiner: Assemble LinkedClasses") {
        val assembled = for {
          (classDef, version) <- classDefs
          if analysis.classInfos.contains(classDef.className)
        } yield {
          BaseLinker.linkClassDef(classDef, version,
              syntheticMethodDefs = Iterator.empty, analysis)
        }

        val (linkedClassDefs, linkedTopLevelExports) = assembled.unzip

        new LinkingUnit(config.coreSpec, linkedClassDefs.toList,
            linkedTopLevelExports.flatten.toList, moduleInitializers)
      }

      inputProvider.cleanAfterRun()

      result
    }
  }

  private def analyze(moduleInitializers: Seq[ModuleInitializer],
      symbolRequirements: SymbolRequirement, logger: Logger)(
      implicit ec: ExecutionContext): Future[Analysis] = {
    for {
      analysis <- Analyzer.computeReachability(config, moduleInitializers,
          symbolRequirements, allowAddingSyntheticMethods = false,
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
}

private object Refiner {
  private class InputProvider extends Analyzer.InputProvider {
    private var classesByName: Map[ClassName, ClassDef] = _
    private val cache = mutable.Map.empty[ClassName, ClassInfoCache]

    def update(classesByName: Map[ClassName, ClassDef]): Unit = {
      this.classesByName = classesByName
    }

    def classesWithEntryPoints(): Iterable[ClassName] = {
      def hasStaticInit(classDef: ClassDef): Boolean = {
        classDef.methods.exists { m =>
          m.flags.namespace == MemberNamespace.StaticConstructor &&
          m.methodName.isStaticInitializer
        }
      }

      classesByName.values
        .withFilter(hasStaticInit(_))
        .map(_.className)
    }

    def loadTopLevelExportInfos()(
        implicit ec: ExecutionContext): Future[List[Infos.TopLevelExportInfo]] = Future {
      /* We do not cache top-level exports, because they're quite rare,
       * and usually quite small when they exist.
       */
      classesByName.values.flatMap(Infos.generateTopLevelExportInfos(_)).toList
    }

    def loadInfo(className: ClassName)(implicit ec: ExecutionContext): Option[Future[Infos.ClassInfo]] =
      getCache(className).map(_.loadInfo(classesByName(className)))

    private def getCache(className: ClassName): Option[ClassInfoCache] = {
      cache.get(className).orElse {
        if (classesByName.contains(className)) {
          val fileCache = new ClassInfoCache
          cache += className -> fileCache
          Some(fileCache)
        } else {
          None
        }
      }
    }

    def cleanAfterRun(): Unit = {
      classesByName = null
      cache.filterInPlace((_, linkedClassCache) => linkedClassCache.cleanAfterRun())
    }
  }

  private class ClassInfoCache {
    private var cacheUsed: Boolean = false
    private val methodsInfoCaches = MethodDefsInfosCache()
    private val jsConstructorInfoCache = new JSConstructorDefInfoCache()
    private val exportedMembersInfoCaches = JSMethodPropDefsInfosCache()
    private var info: Infos.ClassInfo = _

    def loadInfo(classDef: ClassDef)(implicit ec: ExecutionContext): Future[Infos.ClassInfo] = Future {
      update(classDef)
      info
    }

    private def update(classDef: ClassDef): Unit = synchronized {
      if (!cacheUsed) {
        cacheUsed = true

        val builder = new Infos.ClassInfoBuilder(classDef.className,
            classDef.kind, classDef.superClass.map(_.name),
            classDef.interfaces.map(_.name), classDef.jsNativeLoadSpec)

        classDef.fields.foreach {
          case FieldDef(flags, FieldIdent(name), _, ftpe) =>
            if (!flags.namespace.isStatic)
              builder.maybeAddReferencedFieldClass(name, ftpe)

          case _: JSFieldDef =>
            // Nothing to do.
        }

        classDef.methods.foreach { method =>
          builder.addMethod(methodsInfoCaches.getInfo(method))
        }

        classDef.jsConstructor.foreach { jsConstructor =>
          builder.addExportedMember(jsConstructorInfoCache.getInfo(jsConstructor))
        }

        for (info <- exportedMembersInfoCaches.getInfos(classDef.jsMethodProps))
          builder.addExportedMember(info)

        classDef.jsNativeMembers.foreach(builder.addJSNativeMember(_))

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
        jsConstructorInfoCache.cleanAfterRun()
        exportedMembersInfoCaches.cleanAfterRun()
      }
      result
    }
  }

  private final class MethodDefsInfosCache private (
      val caches: Array[mutable.Map[MethodName, MethodDefInfoCache]])
      extends AnyVal {

    def getInfo(methodDef: MethodDef): Infos.MethodInfo = {
      val cache = caches(methodDef.flags.namespace.ordinal)
        .getOrElseUpdate(methodDef.methodName, new MethodDefInfoCache)
      cache.getInfo(methodDef)
    }

    def cleanAfterRun(): Unit = {
      caches.foreach(_.filterInPlace((_, cache) => cache.cleanAfterRun()))
    }
  }

  private object MethodDefsInfosCache {
    def apply(): MethodDefsInfosCache = {
      new MethodDefsInfosCache(
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
  private final class JSMethodPropDefsInfosCache private (
      private var caches: Array[JSMethodPropDefInfoCache]) {

    def getInfos(members: List[JSMethodPropDef]): List[Infos.ReachabilityInfo] = {
      if (members.isEmpty) {
        caches = null
        Nil
      } else {
        val membersSize = members.size
        if (caches == null || membersSize != caches.size)
          caches = Array.fill(membersSize)(new JSMethodPropDefInfoCache)

        for ((member, i) <- members.zipWithIndex) yield {
          caches(i).getInfo(member)
        }
      }
    }

    def cleanAfterRun(): Unit = {
      if (caches != null)
        caches.foreach(_.cleanAfterRun())
    }
  }

  private object JSMethodPropDefsInfosCache {
    def apply(): JSMethodPropDefsInfosCache =
      new JSMethodPropDefsInfosCache(null)
  }

  private abstract class AbstractMemberInfoCache[Def <: VersionedMemberDef, Info] {
    private var cacheUsed: Boolean = false
    private var lastVersion: Version = Version.Unversioned
    private var info: Info = _

    final def getInfo(member: Def): Info = {
      update(member)
      info
    }

    private final def update(member: Def): Unit = {
      if (!cacheUsed) {
        cacheUsed = true
        val newVersion = member.version
        if (!lastVersion.sameVersion(newVersion)) {
          info = computeInfo(member)
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

  private final class MethodDefInfoCache
      extends AbstractMemberInfoCache[MethodDef, Infos.MethodInfo] {

    protected def computeInfo(member: MethodDef): Infos.MethodInfo =
      Infos.generateMethodInfo(member)
  }

  private final class JSConstructorDefInfoCache
      extends AbstractMemberInfoCache[JSConstructorDef, Infos.ReachabilityInfo] {

    protected def computeInfo(member: JSConstructorDef): Infos.ReachabilityInfo =
      Infos.generateJSConstructorInfo(member)
  }

  private final class JSMethodPropDefInfoCache
      extends AbstractMemberInfoCache[JSMethodPropDef, Infos.ReachabilityInfo] {

    protected def computeInfo(member: JSMethodPropDef): Infos.ReachabilityInfo = {
      member match {
        case methodDef: JSMethodDef =>
          Infos.generateJSMethodInfo(methodDef)
        case propertyDef: JSPropertyDef =>
          Infos.generateJSPropertyInfo(propertyDef)
      }
    }
  }
}
