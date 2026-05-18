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

package org.scalajs.sbtplugin

import java.io.File

import sbt.*
import sbt.Keys.*
import sbt.librarymanagement.DependencyResolution
import lmcoursier.{CoursierConfiguration, CoursierDependencyResolution}
import sbt.internal.util.StringAttributeKey
import xsbti.{FileConverter, HashedVirtualFileRef, VirtualFileRef}

import org.scalajs.ir.ScalaJSVersions
import org.scalajs.linker.interface.ModuleKind

private[sbtplugin] object PluginCompat {
  private final val FilePathSeparator = '\u0000'

  val linkerScalaSuffix: String = "_2.13"

  def virtualFileRefToFile(ref: VirtualFileRef)(using conv: FileConverter): File =
    conv.toPath(ref).toFile

  def fileToVirtualFileRef(f: File)(using conv: FileConverter): VirtualFileRef =
    conv.toVirtualFile(f.toPath)

  def withLinkerOutputCache(cacheDirectory: File, reportFile: File, configChanged: Boolean)(
      body: Set[File] => Set[File])(realFiles: Set[File]): Set[File] = {
    body(realFiles)
  }

  def toFiles(cp: Seq[Attributed[HashedVirtualFileRef]])(using conv: FileConverter): Seq[File] =
    cp.map(a => conv.toPath(a.data).toFile)

  def toAttributedFiles(files: Seq[File])(
      using conv: FileConverter): Seq[Attributed[HashedVirtualFileRef]] = {
    Attributed.blankSeq(files.map(f => conv.toVirtualFile(f.toPath)))
  }

  // Store File as path string
  def attributedPutFile[T](a: Attributed[T], key: AttributeKey[File], value: File): Attributed[T] =
    a.put(StringAttributeKey(key.label), value.getAbsolutePath)

  def attributedGetFile[T](a: Attributed[T], key: AttributeKey[File]): Option[File] =
    a.get(StringAttributeKey(key.label)).map(path => new File(path))

  def attributedPutFiles[T](a: Attributed[T], key: AttributeKey[Seq[File]],
      value: Seq[File]): Attributed[T] = {
    a.put(StringAttributeKey(key.label),
        value.map(_.getAbsolutePath).mkString(FilePathSeparator.toString))
  }

  def attributedGetFiles[T](a: Attributed[T], key: AttributeKey[Seq[File]]): Option[Seq[File]] = {
    a.get(StringAttributeKey(key.label)).map { s =>
      if (s.isEmpty) Nil else s.split(FilePathSeparator).toSeq.map(new File(_))
    }
  }

  // no-op, scalaJSModuleKind is not suppported in sbt2
  def attributedPutModuleKind(a: Attributed[File], moduleKind: ModuleKind): Attributed[File] = a

  def dependencyResolutionValue(
      _dependencyResolution: Def.Initialize[Task[DependencyResolution]]
  ): Def.Initialize[Task[DependencyResolution]] = {
    Def.task {
      val log = streams.value.log
      val csrConfig = CoursierConfiguration()
        .withResolvers(Vector(Resolver.defaultLocal, Resolver.mavenCentral))
        .withLog(Some(log))
      CoursierDependencyResolution(csrConfig)
    }
  }

  def platformDepsCrossVersionSetting: Seq[Setting[?]] = Seq(
    platform := "sjs" + ScalaJSVersions.binaryCross
  )

  // In sbt 2.x with platform set, %% adds the platform suffix (_sjs1)
  def scalaJSCoreLib(org: String, name: String, version: String,
      scalaBinaryVersion: String): ModuleID = {
    org % s"${name}_$scalaBinaryVersion" % version
  }

  // In sbt 2.x with platform set, cross CrossVersion.full also adds the platform suffix
  def scalaJSFullCrossVersionLib(org: String, name: String, version: String,
      scalaVersion: String): ModuleID = {
    org % s"${name}_$scalaVersion" % version
  }

  type DefOps = Nothing

}
