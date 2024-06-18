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
import org.scalajs.linker.standard.LinkTimeProperties
import org.scalajs.linker.CollectionsCompat.MutableMapCompatOps

import Platform.emptyThreadSafeMap

private[analyzer] final class InfoLoader(irLoader: IRLoader, irCheckMode: InfoLoader.IRCheckMode,
    linkTimeProperties: LinkTimeProperties) {
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
      Some(infoCache.loadInfo(logger, linkTimeProperties))
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

  private type MethodInfos = Array[Map[MethodName, Infos.MethodInfo]]

  private class ClassInfoCache(className: ClassName, irLoader: IRLoader, irCheckMode: InfoLoader.IRCheckMode) {
    private var cacheUsed: Boolean = false
    private var version: Version = Version.Unversioned
    private var info: Future[Infos.ClassInfo] = _

    private var prevMethodInfos: MethodInfos = Array.fill(MemberNamespace.Count)(Map.empty)
    private var prevJSCtorInfo: Option[Infos.ReachabilityInfo] = None
    private var prevJSMethodPropDefInfos: List[Infos.ReachabilityInfo] = Nil

    def loadInfo(logger: Logger, linkTimeProperties: LinkTimeProperties)(
        implicit ec: ExecutionContext): Future[Infos.ClassInfo] = synchronized {
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
                    postBaseLinker = false, postOptimizer = false,
                    logger, linkTimeProperties)
                if (errorCount != 0) {
                  throw new LinkingException(
                      s"There were $errorCount ClassDef checking errors.")
                }

              case InfoLoader.InternalIRCheck =>
                val errorCount = ClassDefChecker.check(tree,
                    postBaseLinker = true, postOptimizer = true,
                    logger, linkTimeProperties)
                if (errorCount != 0) {
                  throw new LinkingException(
                      s"There were $errorCount ClassDef checking errors after optimizing. " +
                      "Please report this as a bug.")
                }
            }

            generateInfos(tree, linkTimeProperties)
          }
        }
      }

      info
    }

    private def generateInfos(classDef: ClassDef, linkTimeProperties: LinkTimeProperties): Infos.ClassInfo =  {
      val referencedFieldClasses = Infos.genReferencedFieldClasses(classDef.fields)

      prevMethodInfos = genMethodInfos(classDef.methods, prevMethodInfos, linkTimeProperties)
      prevJSCtorInfo = genJSCtorInfo(classDef.jsConstructor, prevJSCtorInfo, linkTimeProperties)
      prevJSMethodPropDefInfos =
        genJSMethodPropDefInfos(classDef.jsMethodProps, prevJSMethodPropDefInfos, linkTimeProperties)

      val exportedMembers = prevJSCtorInfo.toList ::: prevJSMethodPropDefInfos

      /* We do not cache top-level exports, because they're quite rare,
       * and usually quite small when they exist.
       */
      val topLevelExports = classDef.topLevelExportDefs
        .map(Infos.generateTopLevelExportInfo(classDef.name.name, _, linkTimeProperties))

      val jsNativeMembers = classDef.jsNativeMembers
        .map(m => m.name.name -> m.jsNativeLoadSpec).toMap

      new Infos.ClassInfo(classDef.className, classDef.kind,
          classDef.superClass.map(_.name), classDef.interfaces.map(_.name),
          classDef.jsNativeLoadSpec, referencedFieldClasses, prevMethodInfos,
          jsNativeMembers, exportedMembers, topLevelExports)
    }

    /** Returns true if the cache has been used and should be kept. */
    def cleanAfterRun(): Boolean = synchronized {
      val result = cacheUsed
      cacheUsed = false
      result
    }
  }

  private def genMethodInfos(methods: List[MethodDef],
      prevMethodInfos: MethodInfos, linkTimeProperties: LinkTimeProperties): MethodInfos = {

    val builders = Array.fill(MemberNamespace.Count)(Map.newBuilder[MethodName, Infos.MethodInfo])

    methods.foreach { method =>
      val info = prevMethodInfos(method.flags.namespace.ordinal)
        .get(method.methodName)
        .filter(_.version.sameVersion(method.version))
        .getOrElse(Infos.generateMethodInfo(method, linkTimeProperties))

      builders(method.flags.namespace.ordinal) += method.methodName -> info
    }

    builders.map(_.result())
  }

  private def genJSCtorInfo(jsCtor: Option[JSConstructorDef],
      prevJSCtorInfo: Option[Infos.ReachabilityInfo],
      linkTimeProperties: LinkTimeProperties): Option[Infos.ReachabilityInfo] = {
    jsCtor.map { ctor =>
      prevJSCtorInfo
        .filter(_.version.sameVersion(ctor.version))
        .getOrElse(Infos.generateJSConstructorInfo(ctor, linkTimeProperties))
    }
  }

  private def genJSMethodPropDefInfos(jsMethodProps: List[JSMethodPropDef],
      prevJSMethodPropDefInfos: List[Infos.ReachabilityInfo],
      linkTimeProperties: LinkTimeProperties): List[Infos.ReachabilityInfo] = {
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

    if (prevJSMethodPropDefInfos.size != jsMethodProps.size) {
      // Regenerate everything.
      jsMethodProps.map(Infos.generateJSMethodPropDefInfo(_, linkTimeProperties))
    } else {
      for {
        (prevInfo, member) <- prevJSMethodPropDefInfos.zip(jsMethodProps)
      } yield {
        if (prevInfo.version.sameVersion(member.version)) prevInfo
        else Infos.generateJSMethodPropDefInfo(member, linkTimeProperties)
      }
    }
  }
}
