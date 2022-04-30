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

import scala.collection.mutable
import scala.concurrent._

import org.scalajs.linker.analyzer._
import org.scalajs.linker.checker.ClassDefChecker
import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable._
import org.scalajs.linker.CollectionsCompat.MutableMapCompatOps

import org.scalajs.logging._

import org.scalajs.ir
import org.scalajs.ir.Names.ClassName
import org.scalajs.ir.Trees.ClassDef

final class IRLoader(checkIR: Boolean) extends Analyzer.InputProvider
    with MethodSynthesizer.InputProvider {
  private var classNameToFile: collection.Map[ClassName, IRFileImpl] = _
  private var entryPoints: collection.Set[ClassName] = _
  private var logger: Logger = _
  private val cache = mutable.Map.empty[ClassName, ClassDefAndInfoCache]


  def update(irInput: Seq[IRFile], logger: Logger)(implicit ec: ExecutionContext): Future[this.type] = {
    this.logger = logger

    Future.traverse(irInput)(i => IRFileImpl.fromIRFile(i).entryPointsInfo).map { infos =>
      val classNameToFile = mutable.Map.empty[ClassName, IRFileImpl]
      val entryPoints = mutable.Set.empty[ClassName]

      for ((input, info) <- irInput.zip(infos)) {
        // Remove duplicates. Just like the JVM
        if (!classNameToFile.contains(info.className))
          classNameToFile += info.className -> IRFileImpl.fromIRFile(input)

        if (info.hasEntryPoint)
          entryPoints += info.className
      }

      this.classNameToFile = classNameToFile
      this.entryPoints = entryPoints

      this
    }
  }

  def classesWithEntryPoints(): Iterable[ClassName] = entryPoints

  def loadTopLevelExportInfos()(implicit ec: ExecutionContext): Future[List[Infos.TopLevelExportInfo]] = {
    Future.traverse(entryPoints)(get(_, _.topLevelExportInfos))
      .map(_.flatten.toList)
  }

  def loadInfo(className: ClassName)(
      implicit ec: ExecutionContext): Option[Future[Infos.ClassInfo]] = {
    maybeGet(className, _.classInfo)
  }

  def loadClassDefAndVersion(className: ClassName)(
      implicit ec: ExecutionContext): Future[(ClassDef, Option[String])] = {
    get(className, u => (u.classDef, u.version))
  }

  def loadClassDef(className: ClassName)(
      implicit ec: ExecutionContext): Future[ClassDef] = {
    get(className, _.classDef)
  }

  private def get[T](className: ClassName, f: ClassDefAndInfoCache.Update => T)(
      implicit ec: ExecutionContext): Future[T] = {
    maybeGet(className, f).getOrElse {
      throw new AssertionError(s"Cannot load file for class $className")
    }
  }

  private def maybeGet[T](className: ClassName, f: ClassDefAndInfoCache.Update => T)(
      implicit ec: ExecutionContext): Option[Future[T]] = {
    classNameToFile.get(className).map { irFile =>
      cache.getOrElseUpdate(className, new ClassDefAndInfoCache)
        .update(irFile, logger, checkIR).map(f)
    }
  }

  def cleanAfterRun(): Unit = {
    classNameToFile = null
    entryPoints = null
    logger = null
    cache.filterInPlace((_, fileCache) => fileCache.cleanAfterRun())
  }
}

private object ClassDefAndInfoCache {
  final class Update(
      val classDef: ClassDef,
      val classInfo: Infos.ClassInfo,
      val topLevelExportInfos: List[Infos.TopLevelExportInfo],
      val version: Option[String])
}

private final class ClassDefAndInfoCache {
  import ClassDefAndInfoCache.Update

  private var cacheUsed: Boolean = false
  private var version: Option[String] = None
  private var cacheUpdate: Future[Update] = _

  def update(irFile: IRFileImpl, logger: Logger, checkIR: Boolean)(
      implicit ec: ExecutionContext): Future[Update] = synchronized {
    /* If the cache was already used in this run, the classDef and info are
     * already correct, no matter what the versions say.
     */
    if (!cacheUsed) {
      cacheUsed = true

      val newVersion = irFile.version
      if (version.isEmpty || newVersion.isEmpty ||
          version.get != newVersion.get) {
        version = newVersion
        cacheUpdate = irFile.tree.map { tree =>
          if (checkIR) {
            val errorCount = ClassDefChecker.check(tree, logger)
            if (errorCount != 0) {
              throw new LinkingException(
                  s"There were $errorCount ClassDef checking errors.")
            }
          }
          new Update(tree, Infos.generateClassInfo(tree),
              Infos.generateTopLevelExportInfos(tree), version)
        }
      }
    }

    cacheUpdate
  }

  /** Returns true if the cache has been used and should be kept. */
  def cleanAfterRun(): Boolean = synchronized {
    val result = cacheUsed
    cacheUsed = false
    result
  }
}
