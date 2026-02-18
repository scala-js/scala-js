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

import sbt._
import sbt.Keys._
import sbt.librarymanagement.DependencyResolution
import xsbti.FileConverter

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

private[sbtplugin] object PluginCompat {
  type FileRef = File
  type Out = File
  type ArtifactPath = File

  val linkerScalaSuffix: String = "_2.12"

  def virtualFileRefToFile(f: File)(implicit conv: FileConverter): File = f
  def fileToVirtualFileRef(f: File)(implicit conv: FileConverter): File = f

  def toFiles(cp: Seq[Attributed[File]])(implicit conv: FileConverter): Seq[File] =
    Attributed.data(cp)

  def toAttributedFiles(files: Seq[File])(implicit conv: FileConverter): Seq[Attributed[File]] =
    Attributed.blankSeq(files)

  def attributedPutFile[T](a: Attributed[T], key: AttributeKey[File], value: File): Attributed[T] =
    a.put(key, value)

  def attributedGetFile[T](a: Attributed[T], key: AttributeKey[File]): Option[File] =
    a.get(key)

  def attributedPutFiles[T](a: Attributed[T], key: AttributeKey[Seq[File]],
      value: Seq[File]): Attributed[T] = {
    a.put(key, value)
  }

  def attributedGetFiles[T](a: Attributed[T], key: AttributeKey[Seq[File]]): Option[Seq[File]] =
    a.get(key)

  def attributedPutString[T](a: Attributed[T], key: AttributeKey[String],
      value: String): Attributed[T] = {
    a.put(key, value)
  }

  def attributedGetString[T](a: Attributed[T], key: AttributeKey[String]): Option[String] =
    a.get(key)

  def dependencyResolutionValue(
      dependencyResolution: Def.Initialize[Task[DependencyResolution]]
  ): Def.Initialize[Task[DependencyResolution]] = {
    Def.task {
      val log = streams.value.log
      import sbt.librarymanagement.ivy._
      val ivyConfig = InlineIvyConfiguration()
        .withResolvers(Vector(Resolver.defaultLocal, Resolver.mavenCentral))
        .withLog(log)
      IvyDependencyResolution(ivyConfig)
    }
  }

  def platformDepsCrossVersionSetting: Seq[Setting[_]] = Seq(
    platformDepsCrossVersion := ScalaJSCrossVersion.binary
  )

  def scalaJSCoreLib(org: String, name: String, version: String,
      scalaBinaryVersion: String): ModuleID = {
    org %% name % version
  }

  def scalaJSFullCrossVersionLib(org: String, name: String, version: String,
      scalaVersion: String): ModuleID = {
    org % name % version cross CrossVersion.full
  }

  /** Detect if linker config changed since previous run.
   *  sbt 1.x implementation using .previous.
   *  Called from within a Def.taskDyn context with previous values passed in.
   */
  def detectConfigChange(
      _cacheDirectory: File,
      moduleInitFingerprints: Seq[String],
      prevModuleInitFingerprints: Option[Seq[String]],
      linkerConfigFingerprint: String,
      prevLinkerConfigFingerprint: Option[String]
  ): Boolean = {
    val moduleInitChanged = prevModuleInitFingerprints.exists(_ != moduleInitFingerprints)
    val linkerConfigChanged = prevLinkerConfigFingerprint.exists(_ != linkerConfigFingerprint)
    moduleInitChanged || linkerConfigChanged
  }

  // In sbt 1.x, .previous is available on TaskKey via sbt's built-in implicit.
  implicit class TaskKeyPreviousOps[T](private val key: TaskKey[T]) extends AnyVal {}

  // `Def.uncached(...)`
  implicit class DefOps(singleton: Def.type) {
    def uncached[A1](a: A1): A1 = a
  }

}
