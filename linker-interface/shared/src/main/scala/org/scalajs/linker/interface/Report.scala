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

package org.scalajs.linker.interface

import java.io._

import org.scalajs.ir.ScalaJSVersions

import org.scalajs.linker.interface.unstable.ReportImpl

abstract class Report private[interface] {
  def publicModules: Iterable[Report.Module]
}

object Report {
  abstract class Module private[interface] {
    def moduleID: String
    def jsFileName: String
    def sourceMapName: Option[String]
    def moduleKind: ModuleKind
  }

  def serialize(report: Report): Array[Byte] = {
    val bytes = new ByteArrayOutputStream
    val out = new DataOutputStream(bytes)

    new Serializer(out).writeReport(report)

    out.close()
    bytes.toByteArray
  }

  /** Tries to deserialize the given bytes as a [[Report]].
   *
   *  @returns None If the provided bytes are not compatible with the current
   *      linker version, Some(<report>) otherwise.
   */
  def deserialize(bytes: Array[Byte]): Option[Report] = {
    val in = new DataInputStream(new ByteArrayInputStream(bytes))
    new Deserializer(in).readReport()
  }

  private final class Serializer(out: DataOutputStream) {
    import out._

    def writeReport(report: Report): Unit = {
      writeUTF(ScalaJSVersions.current)
      writeInt(report.publicModules.size)
      report.publicModules.foreach(writeModule(_))
    }

    private def writeModule(module: Module): Unit = {
      writeUTF(module.moduleID)
      writeUTF(module.jsFileName)
      writeBoolean(module.sourceMapName.isDefined)
      module.sourceMapName.foreach(writeUTF(_))
      writeModuleKind(module.moduleKind)
    }

    private def writeModuleKind(kind: ModuleKind): Unit = {
      val i = kind match {
        case ModuleKind.NoModule => 0
        case ModuleKind.ESModule => 1
        case ModuleKind.CommonJSModule => 2
      }
      writeByte(i)
    }
  }

  private final class Deserializer(in: DataInputStream) {
    import in._

    def readReport(): Option[Report] = {
      val v = in.readUTF()
      if (v != ScalaJSVersions.current) {
        None
      } else {
        val publicModules = List.fill(in.readInt())(readModule())
        Some(new ReportImpl(publicModules))
      }
    }

    private def readModule(): Module = {
      new ReportImpl.ModuleImpl (
          moduleID = readUTF(),
          jsFileName = readUTF(),
          sourceMapName = readOptString(),
          moduleKind = readModuleKind()
      )
    }

    private def readOptString(): Option[String] =
      if (readBoolean()) Some(readUTF())
      else None

    private def readModuleKind(): ModuleKind = {
      readByte() match {
        case 0 => ModuleKind.NoModule
        case 1 => ModuleKind.ESModule
        case 2 => ModuleKind.CommonJSModule
        case v => throw new IllegalArgumentException(s"unknown module byte: $v")
      }
    }
  }
}
