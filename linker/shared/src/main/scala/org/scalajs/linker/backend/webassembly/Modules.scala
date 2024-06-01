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

package org.scalajs.linker.backend.webassembly

import org.scalajs.ir.{OriginalName, Position}

import Instructions._
import Identitities._
import Types._

/** WebAssembly modules and their structure.
 *
 *  @see
 *    [[https://webassembly.github.io/gc/core/syntax/modules.html]]
 */
object Modules {

  /** A WebAssembly `export`. */
  final case class Export(name: String, desc: ExportDesc)

  /** A WebAssembly `exportdesc`. */
  sealed abstract class ExportDesc

  object ExportDesc {
    final case class Func(id: FunctionID) extends ExportDesc
    final case class Global(id: GlobalID) extends ExportDesc
  }

  /** A WebAssembly `import`. */
  final case class Import(module: String, name: String, desc: ImportDesc)

  /** A WebAssembly `importdesc`. */
  sealed abstract class ImportDesc

  object ImportDesc {
    final case class Func(id: FunctionID, originalName: OriginalName, typeID: TypeID)
        extends ImportDesc

    final case class Global(id: GlobalID, originalName: OriginalName, tpe: Type, isMutable: Boolean)
        extends ImportDesc

    final case class Tag(id: TagID, originalName: OriginalName, typeID: TypeID) extends ImportDesc
  }

  /** A WebAssembly `func`, including names for parameters and locals. */
  final case class Function(
      id: FunctionID,
      originalName: OriginalName,
      typeID: TypeID,
      params: List[Local],
      results: List[Type],
      locals: List[Local],
      body: Expr,
      pos: Position
  )

  /** A local variable declaration within a `Function`. */
  final case class Local(id: LocalID, originalName: OriginalName, tpe: Type)

  /** A WebAssembly `tag` definition. */
  final case class Tag(id: TagID, originalName: OriginalName, typeID: TypeID)

  /** A WebAssembly `data` definition. */
  final case class Data(id: DataID, originalName: OriginalName, bytes: Array[Byte], mode: Data.Mode)

  object Data {
    sealed abstract class Mode

    object Mode {
      case object Passive extends Mode
      // final case class Active(...)
    }
  }

  /** A WebAssembly `global` definition. */
  final case class Global(
      id: GlobalID,
      originalName: OriginalName,
      tpe: Type,
      init: Expr,
      isMutable: Boolean
  )

  /** A WebAssembly `elem` definition. */
  final case class Element(tpe: Type, init: List[Expr], mode: Element.Mode)

  object Element {
    sealed abstract class Mode

    object Mode {
      case object Passive extends Mode
      // final case class Active(table: Immediate.TableIdx, offset: Expr) extends Mode
      case object Declarative extends Mode
    }
  }

  /** A WebAssembly `module`. */
  final class Module(
      val types: List[RecType],
      val imports: List[Import],
      val funcs: List[Function],
      val tags: List[Tag],
      val globals: List[Global],
      val exports: List[Export],
      val start: Option[FunctionID],
      val elems: List[Element],
      val datas: List[Data]
  )
}
