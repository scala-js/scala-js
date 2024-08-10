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
 *
 *  @see
 *    For tags:
 *    [[https://webassembly.github.io/exception-handling/core/syntax/modules.html]]
 */
object Modules {

  /** A WebAssembly `import`. */
  final case class Import(module: String, name: String, desc: ImportDesc)

  /** A WebAssembly `importdesc`. */
  sealed abstract class ImportDesc

  object ImportDesc {
    final case class Func(id: FunctionID, originalName: OriginalName, typeID: TypeID)
        extends ImportDesc

    final case class Global(id: GlobalID, originalName: OriginalName, isMutable: Boolean, tpe: Type)
        extends ImportDesc

    final case class Tag(id: TagID, originalName: OriginalName, typeID: TypeID) extends ImportDesc
  }

  /** A WebAssembly `func`, including names/types for parameters, locals and results.
   *
   *  @note
   *    The `params`' types and the `results` are not strictly necessary, as
   *    they can be derived from the `typeID` by resolving it to a function
   *    type. The binary writer ignores them. They are used by the text writer.
   */
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

  /** A WebAssembly `global` definition. */
  final case class Global(
      id: GlobalID,
      originalName: OriginalName,
      isMutable: Boolean,
      tpe: Type,
      init: Expr
  )

  /** A WebAssembly `export`.
   *
   *  @note
   *    We do not use any `export` in our current compilation scheme.
   *    However, we used them in the past and we will likely reuse them in the
   *    future (notably for module splitting). Therefore, we keep them in the
   *    codebase not to lose the work done in implementing them.
   */
  final case class Export(name: String, desc: ExportDesc)

  /** A WebAssembly `exportdesc`. */
  sealed abstract class ExportDesc

  object ExportDesc {
    final case class Func(id: FunctionID) extends ExportDesc
    final case class Global(id: GlobalID) extends ExportDesc
  }

  /** A WebAssembly `elem` definition. */
  final case class Element(tpe: Type, init: List[Expr], mode: Element.Mode)

  object Element {
    sealed abstract class Mode

    object Mode {
      case object Declarative extends Mode
    }
  }

  /** A WebAssembly `data` segment definition. */
  final case class Data(id: DataID, originalName: OriginalName, bytes: Array[Byte], mode: Data.Mode)

  object Data {
    sealed abstract class Mode

    object Mode {
      case object Passive extends Mode
    }
  }

  /** A WebAssembly `module`.
   *
   *  Fields are declared in the order of the binary format:
   *  [[https://webassembly.github.io/gc/core/binary/modules.html#sections]]
   */
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
