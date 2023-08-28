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

import scala.concurrent._

import scala.collection.mutable

import org.scalajs.ir.{EntryPointsInfo, Version}
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._

import org.scalajs.logging._

import org.scalajs.linker.checker.ClassDefChecker
import org.scalajs.linker.frontend.IRLoader
import org.scalajs.linker.interface.LinkingException
import org.scalajs.linker.CollectionsCompat.MutableMapCompatOps

import Platform.emptyThreadSafeMap

private[analyzer] final class InfoLoader(irLoader: IRLoader, irCheckMode: InfoLoader.IRCheckMode) {
  private var logger: Logger = _
  private val cache = emptyThreadSafeMap[ClassName, InfoLoader.ClassInfoCache]

  def update(logger: Logger): Unit = {
    this.logger = logger
  }

  def classesWithEntryPoints(): Iterable[ClassName] =
    irLoader.classesWithEntryPoints()

  def loadInfo(className: ClassName)(
      implicit ec: ExecutionContext): Option[Future[Infos.ClassInfo]] = {
    if (irLoader.classExists(className)) {
      val infoCache = cache.getOrElseUpdate(className,
          new InfoLoader.ClassInfoCache(className, irLoader, irCheckMode))
      Some(infoCache.loadInfo(logger))
    } else {
      None
    }
  }

  def cleanAfterRun(): Unit = {
    logger = null
    cache.filterInPlace((_, infoCache) => infoCache.cleanAfterRun())
  }
}

private[analyzer] object InfoLoader {
  sealed trait IRCheckMode

  case object NoIRCheck extends IRCheckMode
  case object InitialIRCheck extends IRCheckMode
  case object InternalIRCheck extends IRCheckMode

  private class ClassInfoCache(className: ClassName, irLoader: IRLoader, irCheckMode: InfoLoader.IRCheckMode) {
    private var cacheUsed: Boolean = false
    private var version: Version = Version.Unversioned
    private var info: Future[Infos.ClassInfo] = _

    private val methodsInfoCaches = MethodDefsInfosCache()
    private val jsConstructorInfoCache = new JSConstructorDefInfoCache()
    private val exportedMembersInfoCaches = JSMethodPropDefsInfosCache()

    def loadInfo(logger: Logger)(implicit ec: ExecutionContext): Future[Infos.ClassInfo] = synchronized {
      /* If the cache was already used in this run, the classDef and info are
       * already correct, no matter what the versions say.
       */
      if (!cacheUsed) {
        cacheUsed = true

        val newVersion = irLoader.irFileVersion(className)
        if (!version.sameVersion(newVersion)) {
          version = newVersion
          info = irLoader.loadClassDef(className).map { tree =>
            irCheckMode match {
              case InfoLoader.NoIRCheck =>
                // no check

              case InfoLoader.InitialIRCheck =>
                val errorCount = ClassDefChecker.check(tree,
                    postBaseLinker = false, postOptimizer = false, logger)
                if (errorCount != 0) {
                  throw new LinkingException(
                      s"There were $errorCount ClassDef checking errors.")
                }

              case InfoLoader.InternalIRCheck =>
                val errorCount = ClassDefChecker.check(tree,
                    postBaseLinker = true, postOptimizer = true, logger)
                if (errorCount != 0) {
                  throw new LinkingException(
                      s"There were $errorCount ClassDef checking errors after optimizing. " +
                      "Please report this as a bug.")
                }
            }

            generateInfos(tree)
          }
        }
      }

      info
    }

    private def generateInfos(classDef: ClassDef): Infos.ClassInfo =  {
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

      /* We do not cache top-level exports, because they're quite rare,
       * and usually quite small when they exist.
       */
      classDef.topLevelExportDefs.foreach { topLevelExportDef =>
        builder.addTopLevelExport(Infos.generateTopLevelExportInfo(classDef.name.name, topLevelExportDef))
      }

      classDef.jsNativeMembers.foreach(builder.addJSNativeMember(_))

      builder.result()
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
