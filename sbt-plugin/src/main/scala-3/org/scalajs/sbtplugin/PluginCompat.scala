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

  // TODO: used for scalaJSModuleKind, but there's no get
  def attributedPutValue[T, V](a: Attributed[T], key: AttributeKey[V], value: V): Attributed[T] =
    a.put(StringAttributeKey(key.label), value.toString)

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

  // Platform deps cross version setting - not needed in sbt 2.x
  // In sbt 2.x, %% is platform-aware by default
  def platformDepsCrossVersionSetting: Seq[Setting[?]] = Seq.empty

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

  private[sbtplugin] trait JsonFormats {
    import sjsonnew._

    import org.scalajs.linker.interface.ModuleInitializer
    import org.scalajs.linker.interface.unstable.ModuleInitializerImpl
    import org.scalajs.linker.interface.unstable.ModuleInitializerImpl._

    /** Hand-written JsonFormat for ModuleInitializer.
     *
     *  We hand-write JsonFormat[ModuelInitializer] instead of using sjson-new's auto-derivation
     *  because ModuleInitializer internally uses `MethodName` (which contains `List[TypeRef]`)
     *  for parameter and return types. Serializing the full `MethodName` would require JsonFormat
     *  instances for the IR type hierarchy (`TypeRef`, `ClassName`, etc.).
     *
     *  However, for ModuleInitializer, we don't need the full `MethodName`, because
     *  MainMethod always has a fixed signature (`() -> Unit or Array[String] -> Unit`).
     *  We only need to serialize the simple method name string (e.g., "main"), and
     *  reconstruct `ModuleInitializer`s using factory methods like `mainMethod`.
     */
    given moduleInitializerJsonFormat: JsonFormat[ModuleInitializer] = {
      new JsonFormat[ModuleInitializer] {
        def write[J](x: ModuleInitializer, builder: Builder[J]): Unit = {
          builder.beginObject()
          builder.addField("moduleID", x.moduleID)
          ModuleInitializerImpl.fromInitializer(x.initializer) match {
            case VoidMainMethod(className, methodName) =>
              builder.addField("type", "VoidMainMethod")
              builder.addField("className", className.nameString)
              builder.addField("mainMethodName", methodName.simpleName.nameString)
            case MainMethodWithArgs(className, methodName, args) =>
              builder.addField("type", "MainMethodWithArgs")
              builder.addField("className", className.nameString)
              builder.addField("mainMethodName", methodName.simpleName.nameString)
              builder.addField("args", args)
          }
          builder.endObject()
        }

        def read[J](jsOpt: Option[J], unbuilder: Unbuilder[J]): ModuleInitializer = {
          jsOpt match {
            case Some(js) =>
              unbuilder.beginObject(js)
              val moduleID = unbuilder.readField[String]("moduleID")
              val tpe = unbuilder.readField[String]("type")
              val className = unbuilder.readField[String]("className")
              val mainMethodName = unbuilder.readField[String]("mainMethodName")
              val base = tpe match {
                case "VoidMainMethod" =>
                  ModuleInitializer.mainMethod(className, mainMethodName)
                case "MainMethodWithArgs" =>
                  val args = unbuilder.readField[List[String]]("args")
                  ModuleInitializer.mainMethodWithArgs(className, mainMethodName, args)
              }
              unbuilder.endObject()
              base.withModuleID(moduleID)
            case None =>
              deserializationError("Expected ModuleInitializer")
          }
        }
      }
    }
  }
}
