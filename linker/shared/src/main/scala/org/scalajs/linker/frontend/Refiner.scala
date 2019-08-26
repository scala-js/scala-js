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
      Map(unit.classDefs.map(c => c.encodedName -> c): _*)
    inputProvider.update(linkedClassesByName)

    val analysis = logger.timeFuture("Refiner: Compute reachability") {
      val allSymbolRequirements = {
        symbolRequirements ++
        ModuleInitializer.toSymbolRequirement(unit.moduleInitializers)
      }

      analyze(allSymbolRequirements, logger)
    }

    for {
      analysis <- analysis
    } yield {
      val result = logger.time("Refiner: Assemble LinkedClasses") {
        val linkedClassDefs = for {
          info <- analysis.classInfos.values
        } yield {
          refineClassDef(linkedClassesByName(info.encodedName), info)
        }

        new LinkingUnit(unit.coreSpec, linkedClassDefs.toList, unit.moduleInitializers)
      }

      inputProvider.cleanAfterRun()

      result
    }
  }

  private def analyze(symbolRequirements: SymbolRequirement, logger: Logger)(
      implicit ec: ExecutionContext): Future[Analysis] = {
    for {
      analysis <- Analyzer.computeReachability(config, symbolRequirements,
          allowAddingSyntheticMethods = false, inputProvider)
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
      info.methodInfos(methodDef.flags.namespace)(
          methodDef.encodedName).isReachable
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
    private var linkedClassesByName: Map[String, LinkedClass] = _
    private val cache = mutable.Map.empty[String, LinkedClassInfoCache]

    def update(linkedClassesByName: Map[String, LinkedClass]): Unit = {
      this.linkedClassesByName = linkedClassesByName
    }

    def classesWithEntryPoints(): Iterable[String] = {
      (for {
        linkedClass <- linkedClassesByName.valuesIterator
        if linkedClass.hasEntryPoint
      } yield {
        linkedClass.encodedName
      }).toList
    }

    def loadInfo(encodedName: String)(implicit ec: ExecutionContext): Option[Future[Infos.ClassInfo]] =
      getCache(encodedName).map(_.loadInfo(linkedClassesByName(encodedName)))

    private def getCache(encodedName: String): Option[LinkedClassInfoCache] = {
      cache.get(encodedName).orElse {
        if (linkedClassesByName.contains(encodedName)) {
          val fileCache = new LinkedClassInfoCache
          cache += encodedName -> fileCache
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
    private val methodsInfoCaches = LinkedMembersInfosCache()
    private val exportedMembersInfoCaches = LinkedMembersInfosCache()
    private var info: Infos.ClassInfo = _

    def loadInfo(linkedClass: LinkedClass)(implicit ec: ExecutionContext): Future[Infos.ClassInfo] = Future {
      update(linkedClass)
      info
    }

    private def update(linkedClass: LinkedClass): Unit = synchronized {
      if (!cacheUsed) {
        cacheUsed = true

        val builder = new Infos.ClassInfoBuilder()
          .setEncodedName(linkedClass.encodedName)
          .setKind(linkedClass.kind)
          .setSuperClass(linkedClass.superClass.map(_.name))
          .addInterfaces(linkedClass.interfaces.map(_.name))

        for (field <- linkedClass.fields)
          builder.maybeAddReferencedFieldClass(field.ftpe)
        for (linkedMethod <- linkedClass.methods)
          builder.addMethod(methodsInfoCaches.getInfo(linkedMethod))
        for (linkedMember <- linkedClass.exportedMembers)
          builder.addMethod(exportedMembersInfoCaches.getInfo(linkedMember))

        if (linkedClass.topLevelExports.nonEmpty) {
          /* We do not cache top-level exports, because they're quite rare,
           * and usually quite small when they exist.
           */
          builder.setIsExported(true)

          val optInfo = Infos.generateTopLevelExportsInfo(
              linkedClass.encodedName, linkedClass.topLevelExports.map(_.value))
          optInfo.foreach(builder.addMethod(_))
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

  private final class LinkedMembersInfosCache private (
      val caches: Array[mutable.Map[String, LinkedMemberInfoCache]])
      extends AnyVal {

    def getInfo(member: Versioned[MemberDef]): Infos.MethodInfo = {
      val memberDef = member.value
      val cache = caches(memberDef.flags.namespace.ordinal)
        .getOrElseUpdate(memberDef.encodedName, new LinkedMemberInfoCache)
      cache.getInfo(member)
    }

    def cleanAfterRun(): Unit = {
      caches.foreach(_.filterInPlace((_, cache) => cache.cleanAfterRun()))
    }
  }

  private object LinkedMembersInfosCache {
    def apply(): LinkedMembersInfosCache = {
      new LinkedMembersInfosCache(
          Array.fill(MemberNamespace.Count)(mutable.Map.empty))
    }
  }

  private final class LinkedMemberInfoCache {
    private var cacheUsed: Boolean = false
    private var lastVersion: Option[String] = None
    private var info: Infos.MethodInfo = _

    def getInfo(member: Versioned[MemberDef]): Infos.MethodInfo = {
      update(member)
      info
    }

    def update(member: Versioned[MemberDef]): Unit = {
      if (!cacheUsed) {
        cacheUsed = true

        val newVersion = member.version
        if (!versionsMatch(newVersion, lastVersion)) {
          info = member.value match {
            case _: FieldDef =>
              throw new AssertionError(
                  "A LinkedMemberInfoCache cannot be used for a FieldDef")
            case methodDef: MethodDef =>
              Infos.generateMethodInfo(methodDef)
            case propertyDef: PropertyDef =>
              Infos.generatePropertyInfo(propertyDef)
          }
          lastVersion = newVersion
        }
      }
    }

    /** Returns true if the cache has been used and should be kept. */
    def cleanAfterRun(): Boolean = {
      val result = cacheUsed
      cacheUsed = false
      result
    }
  }

  private def versionsMatch(a: Option[String], b: Option[String]): Boolean =
    a.isDefined && b.isDefined && a.get == b.get
}
