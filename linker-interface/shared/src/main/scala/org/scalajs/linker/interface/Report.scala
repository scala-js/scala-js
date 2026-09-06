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

import scala.annotation.nowarn

import scala.collection.immutable

import java.io._

import org.scalajs.ir.ScalaJSVersions

import org.scalajs.linker.interface.unstable.ReportImpl

/** Information about a linker run. */
abstract class Report private[interface] () {

  /** Public modules resulting from linking.
   *
   *  An incremental linker must return all public modules here, even if some
   *  were not re-written during this linking run.
   */
  def publicModules: immutable.Iterable[Report.Module]

  override def toString(): String = {
    s"""Report(
       |  publicModules = [
       |    ${publicModules.mkString(",\n")}
       |  ],
       |)""".stripMargin
  }
}

object Report {

  /** Information about a module produced by the linker. */
  abstract class Module private[interface] () {

    /** The module ID of this module. */
    def moduleID: String

    /** The name of the file in the linker's [[OutputDirectory]] that is the
     *  entry point for this module.
     *
     *  For JS module kinds, this is a JavaScript file. For Wasm-only module
     *  kinds, it is a `.wasm` file.
     */
    def moduleFileName: String

    /** The name of the JS file in the linker's [[OutputDirectory]].
     *
     *  Deprecated alias for `moduleFileName`.
     *
     *  This method is misleading for Wasm-only module kinds, such as
     *  `ModuleKind.MinimalWasm`. It returns a `.wasm` file, not a JavaScript
     *  file.
     */
    @deprecated("jsFileName is misleading for Wasm-only module kinds; use moduleFileName instead",
        since = "1.23.0")
    def jsFileName: String = moduleFileName

    /** The name of the source map (if one was generated) in the linker's
     *  [[OutputDirectory]].
     */
    @deprecated(
        "sourceMapName is meaningless when targeting WebAssembly " +
        "(including with ModuleKind.ESModule); " +
        "accurate information can be found inside the JS or Wasm module files",
        since = "1.23.0")
    def sourceMapName: Option[String]

    /** The [[ModuleKind]] of this module. */
    def moduleKind: ModuleKind

    override def toString(): String = {
      s"""Module(
         |  moduleID       = $moduleID,
         |  moduleFileName = $moduleFileName,
         |  moduleKind     = $moduleKind,
         |)""".stripMargin
    }
  }

  /** Serializes this [[Report]] to a byte array.
   *
   *  A report serialized with the exact same linker version is guaranteed to be
   *  deserializable using [[deserialize]]. If the linker version is different,
   *  no guarantee is given.
   */
  def serialize(report: Report): Array[Byte] = {
    val bytes = new ByteArrayOutputStream
    val out = new DataOutputStream(bytes)

    new Serializer(out).writeReport(report)

    out.close()
    bytes.toByteArray
  }

  /** Tries to deserialize the given bytes as a [[Report]].
   *
   *  @return `None` If the provided bytes are not compatible with the current
   *      linker version, `Some(<report>)` otherwise.
   *  @see [[serialize]] about when this is guaranteed to return `Some(<report>)`.
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
      writeUTF(module.moduleFileName)

      val sourceMapName = module.sourceMapName: @nowarn("cat=deprecation")
      writeBoolean(sourceMapName.isDefined)
      sourceMapName.foreach(writeUTF(_))

      writeModuleKind(module.moduleKind)
    }

    private def writeModuleKind(kind: ModuleKind): Unit = {
      val i = kind match {
        case ModuleKind.NoModule       => 0
        case ModuleKind.ESModule       => 1
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
      new ReportImpl.ModuleImpl(
        moduleID = readUTF(),
        moduleFileName = readUTF(),
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
