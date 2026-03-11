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
import xsbti.{FileConverter, HashedVirtualFileRef, VirtualFile, VirtualFileRef}

private[sbtplugin] object PluginCompat {
  type FileRef = HashedVirtualFileRef
  type Out = VirtualFile
  type ArtifactPath = VirtualFileRef

  private val FilePathSeparator = "\u0000"

  val linkerScalaSuffix: String = "_2.13"

  def virtualFileRefToFile(ref: VirtualFileRef)(using conv: FileConverter): File =
    conv.toPath(ref).toFile

  def fileToVirtualFileRef(f: File)(using conv: FileConverter): VirtualFileRef =
    conv.toVirtualFile(f.toPath)

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
    a.put(StringAttributeKey(key.label), value.map(_.getAbsolutePath).mkString(FilePathSeparator))
  }

  def attributedGetFiles[T](a: Attributed[T], key: AttributeKey[Seq[File]]): Option[Seq[File]] = {
    a.get(StringAttributeKey(key.label)).map(
        s => if (s.isEmpty) Nil else s.split(FilePathSeparator).toSeq.map(new File(_)))
  }

  def attributedPutString[T](a: Attributed[T], key: AttributeKey[String],
      value: String): Attributed[T] = {
    a.put(StringAttributeKey(key.label), value)
  }

  def attributedGetString[T](a: Attributed[T], key: AttributeKey[String]): Option[String] =
    a.get(StringAttributeKey(key.label))

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
    platform := ScalaJSCrossVersion.binaryPlatform
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

  /** Detect if linker config changed since previous run.
   *
   *  sbt 2.x implementation: Uses file-based caching to store fingerprints on disk.
   *  The .previous macro from sbt 1.x doesn't exist in sbt 2.x, so we manually
   *  persist fingerprints to files and compare them on subsequent runs.
   *
   *  Note: In the future, sbt 2.x's built-in remote/disk caching mechanism may
   *  handle this automatically, at which point this workaround could be removed.
   *  For now, we need this to ensure config changes trigger re-linking.
   */
  def detectConfigChange(
      cacheDirectory: File,
      moduleInitFingerprints: Seq[String],
      prevModuleInitFingerprints: Option[Seq[String]],
      linkerConfigFingerprint: String,
      prevLinkerConfigFingerprint: Option[String]
  ): Boolean = {
    val moduleInitFingerprintFile = cacheDirectory / "module-init-fingerprints.txt"
    val linkerConfigFingerprintFile = cacheDirectory / "linker-config-fingerprint.txt"

    def fingerprintChanged(file: File, current: String): Boolean = {
      if (file.exists()) {
        val previous = IO.read(file)
        previous != current
      } else {
        false
      }
    }

    val moduleInitStr = moduleInitFingerprints.mkString(",")
    val moduleInitializersChanged = fingerprintChanged(moduleInitFingerprintFile, moduleInitStr)
    val linkerConfigChanged =
      fingerprintChanged(linkerConfigFingerprintFile, linkerConfigFingerprint)

    // Update stored fingerprints for next run
    IO.write(moduleInitFingerprintFile, moduleInitStr)
    IO.write(linkerConfigFingerprintFile, linkerConfigFingerprint)

    moduleInitializersChanged || linkerConfigChanged
  }

  /** Extension to provide dummy .previous on TaskKey in sbt 2.x. */
  implicit class TaskKeyPreviousOps[T](private val key: TaskKey[T]) extends AnyVal {
    def previous: Option[T] = None
  }

  implicit class DefOps(singleton: Def.type)

}
