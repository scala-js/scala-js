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

import org.scalajs.ir.Version
import org.scalajs.ir.Names.ClassName
import org.scalajs.ir.Trees.ClassDef

trait IRLoader extends MethodSynthesizer.InputProvider {
  def classesWithEntryPoints(): Iterable[ClassName]
  def classExists(className: ClassName): Boolean
  def irFileVersion(className: ClassName): Version
  def loadClassDef(className: ClassName)(
      implicit ec: ExecutionContext): Future[ClassDef]

  def synthesizeClass(syntheticKind: SyntheticClassKind): ClassName
}

final class FileIRLoader extends IRLoader {
  private var classNameToFile: collection.Map[ClassName, IRFileImpl] = _
  private var entryPoints: collection.Set[ClassName] = _
  private var syntheticClasses: java.util.concurrent.ConcurrentHashMap[SyntheticClassKind, ClassDef] = _
  private var classNameToSyntheticClass: java.util.concurrent.ConcurrentHashMap[ClassName, ClassDef] = _

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
      this.syntheticClasses = new java.util.concurrent.ConcurrentHashMap()
      this.classNameToSyntheticClass = new java.util.concurrent.ConcurrentHashMap()

      this
    }
  }

  def classesWithEntryPoints(): Iterable[ClassName] = entryPoints

  def classExists(className: ClassName): Boolean =
    classNameToFile.contains(className) || classNameToSyntheticClass.containsKey(className)

  def irFileVersion(className: ClassName): Version = {
    classNameToFile.get(className) match {
      case Some(irFile) => irFile.version
      case None         => SyntheticClassKind.constantVersion
    }
  }

  def loadClassDef(className: ClassName)(
      implicit ec: ExecutionContext): Future[ClassDef] = {
    classNameToFile.get(className) match {
      case Some(irFile) => irFile.tree
      case None         => Future.successful(classNameToSyntheticClass.get(className))
    }
  }

  def synthesizeClass(syntheticKind: SyntheticClassKind): ClassName = {
    syntheticClasses.computeIfAbsent(syntheticKind, { syntheticKind =>
      val classDef = syntheticKind.synthesize()
      classNameToSyntheticClass.put(classDef.className, classDef)
      classDef
    }).className
  }

  def getAllSynthesizedClass(): Map[SyntheticClassKind, ClassName] = {
    val b = Map.newBuilder[SyntheticClassKind, ClassName]
    syntheticClasses.forEach { (syntheticKind, classDef) =>
      b += syntheticKind -> classDef.className
    }
    b.result()
  }

  def cleanAfterRun(): Unit = {
    classNameToFile = null
    entryPoints = null
    syntheticClasses = null
  }
}
