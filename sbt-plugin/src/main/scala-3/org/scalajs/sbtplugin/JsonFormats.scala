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

import java.nio.file.Paths

import sbt.Attributed
import sbt.util.StringStrings.given
import sjsonnew._
import sjsonnew.BasicJsonProtocol._
import xsbti.FileConverter

import org.scalajs.ir.Version
import org.scalajs.linker.interface.{IRFile, ModuleInitializer, StandardConfig}
import org.scalajs.linker.interface.unstable.IRFileImpl
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl._

trait JsonFormats {

  given virtualFileRefHashWriter: HashWriter[xsbti.VirtualFileRef] with {
    def write[J](x: xsbti.VirtualFileRef, builder: Builder[J]): Unit =
      builder.writeString(x.id())
  }

  given versionHashWriter: HashWriter[Version] with {
    def write[J](x: Version, builder: Builder[J]): Unit =
      builder.writeString(x.toString)
  }

  /* This mirrors the current path encoding used by the linker implementation
   * in `PathIRContainer`: IR files use their file path,
   * or IR files inside JARs use "<jar path>:<entry path>".
   * Ideally, the linker interface should expose a portable cache identity for
   * IRFile so this logic is not duplicated here (?)
   */
  given irFileHashWriter(using conv: FileConverter): HashWriter[IRFile] with {
    def write[J](x: IRFile, builder: Builder[J]): Unit = {
      val impl = IRFileImpl.fromIRFile(x)
      val path = impl.path
      val portablePath = {
        val idx = path.indexOf(".jar:")
        if (idx >= 0) {
          val jarEnd = idx + ".jar".length
          val jarPath = path.substring(0, jarEnd)
          val entry = path.substring(jarEnd) // includes the leading ':'
          s"${conv.toVirtualFile(Paths.get(jarPath)).id}$entry"
        } else {
          conv.toVirtualFile(Paths.get(path)).id
        }
      }

      builder.beginObject()
      builder.addField("path", portablePath)
      builder.addField("version", impl.version.toString)
      builder.endObject()
    }
  }

  given attributedHashWriter[T](using inner: HashWriter[T]): HashWriter[Attributed[T]] with {
    // Only the data is currently relevant to fastLinkJS/fullLinkJS.
    // If attributes affect linker semantics, they must be hashed explicitly here?
    def write[J](x: Attributed[T], builder: Builder[J]): Unit =
      inner.write(x.data, builder)
  }

  given standardConfigHashWriter: HashWriter[StandardConfig] with {
    def write[J](x: StandardConfig, builder: Builder[J]): Unit =
      builder.writeString(StandardConfig.fingerprint(x))
  }

  // HashWriter[T] can be derived from `JsonFormats`, but
  // there's no inductive auto derivation is provided by sjson-new.

  given seqHashWriter[T](using inner: HashWriter[T]): HashWriter[Seq[T]] with {
    def write[J](x: Seq[T], builder: Builder[J]): Unit = {
      builder.beginObject()
      x.iterator.zipWithIndex.foreach { case (item, index) =>
        inner.addField(index.toString, item, builder)
      }
      builder.endObject()
    }
  }

  given emptyTupleHashWriter: HashWriter[EmptyTuple] with {
    def write[J](x: EmptyTuple, builder: Builder[J]): Unit = {
      builder.beginArray()
      builder.endArray()
    }
  }

  given consTupleHashWriter[H, T <: Tuple](
      using H: HashWriter[H], T: HashWriter[T]): HashWriter[H *: T] with {
    def write[J](x: H *: T, builder: Builder[J]): Unit = {
      builder.beginArray()
      H.write(x.head, builder)
      T.write(x.tail, builder)
      builder.endArray()
    }
  }

  /** Hand-written JsonFormat for ModuleInitializer.
   *
   *  We hand-write JsonFormat[ModuleInitializer] instead of using sjson-new's
   *  auto-derivation because ModuleInitializer internally uses `MethodName`
   *  (which contains `List[TypeRef]`) for parameter and return types.
   *  Serializing the full `MethodName` would require JsonFormat instances for
   *  the IR type hierarchy (`TypeRef`, `ClassName`, etc.).
   *
   *  However, for ModuleInitializer, we don't need the full `MethodName`,
   *  because MainMethod always has a fixed signature
   *  (`() -> Unit or Array[String] -> Unit`). We only need to serialize the
   *  simple method name string (e.g., "main"), and reconstruct
   *  `ModuleInitializer`s using factory methods like `mainMethod`.
   */
  implicit val moduleInitializerJsonFormat: JsonFormat[ModuleInitializer] = {
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
