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
import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable._
import org.scalajs.linker.CollectionsCompat.MutableMapCompatOps

import org.scalajs.ir
import org.scalajs.ir.Names.ClassName
import org.scalajs.ir.Trees.ClassDef

final class IRLoader extends Analyzer.InputProvider with MethodSynthesizer.InputProvider {
  private var classNameToFile: collection.Map[ClassName, IRFileImpl] = _
  private var entryPoints: collection.Set[ClassName] = _
  private val cache = mutable.Map.empty[ClassName, ClassDefAndInfoCache]

  def update(irInput: Seq[IRFile])(implicit ec: ExecutionContext): Future[this.type] = {
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

  def loadInfo(className: ClassName)(
      implicit ec: ExecutionContext): Option[Future[Infos.ClassInfo]] =
    getCache(className).map(_.loadInfo(classNameToFile(className)))

  def loadClassDefAndVersion(className: ClassName)(
      implicit ec: ExecutionContext): Future[(ClassDef, Option[String])] = {
    val fileCache = getCache(className).getOrElse {
      throw new AssertionError(s"Cannot load file for class $className")
    }
    fileCache.loadClassDefAndVersion(classNameToFile(className))
  }

  def loadClassDef(className: ClassName)(
      implicit ec: ExecutionContext): Future[ClassDef] = {
    loadClassDefAndVersion(className).map(_._1)
  }

  private def getCache(className: ClassName): Option[ClassDefAndInfoCache] = {
    cache.get(className).orElse {
      if (classNameToFile.contains(className)) {
        val fileCache = new ClassDefAndInfoCache
        cache += className -> fileCache
        Some(fileCache)
      } else {
        None
      }
    }
  }

  def cleanAfterRun(): Unit = {
    classNameToFile = null
    entryPoints = null
    cache.filterInPlace((_, fileCache) => fileCache.cleanAfterRun())
  }
}

private final class ClassDefAndInfoCache {
  private var cacheUsed: Boolean = false
  private var version: Option[String] = None
  private var cacheUpdate: Future[(ClassDef, Infos.ClassInfo)] = _

  def loadInfo(irFile: IRFileImpl)(
      implicit ec: ExecutionContext): Future[Infos.ClassInfo] = {
    update(irFile).map(_._2)
  }

  def loadClassDefAndVersion(irFile: IRFileImpl)(
      implicit ec: ExecutionContext): Future[(ClassDef, Option[String])] = {
    update(irFile).map(s => (s._1, version))
  }

  private def update(irFile: IRFileImpl)(
      implicit ec: ExecutionContext): Future[(ClassDef, Infos.ClassInfo)] = synchronized {
    /* If the cache was already used in this run, the classDef and info are
     * already correct, no matter what the versions say.
     */
    if (!cacheUsed) {
      cacheUsed = true

      val newVersion = irFile.version
      if (version.isEmpty || newVersion.isEmpty ||
          version.get != newVersion.get) {
        version = newVersion
        cacheUpdate = irFile.tree.map(t => (t, Infos.generateClassInfo(t)))
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
