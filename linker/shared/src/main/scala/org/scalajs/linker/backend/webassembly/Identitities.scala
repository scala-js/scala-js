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

/** Abstract representation of WebAssembly indices as IDs.
 *
 *  Concrete implementations must provide meaningful `equals` and `hashCode` semantics.
 *
 *  They are encouraged to have a usable `toString()` implementation for debugging purposes.
 *
 *  @see
 *    [[https://webassembly.github.io/gc/core/syntax/modules.html#indices]]
 */
object Identitities {
  /** ID of a type declaration.
   *
   *  Although type declarations are actually `subtype`s and are defined within
   *  `rectype`s, `TypeID`s are not scoped; they are global.
   */
  trait TypeID

  /** ID of a field within an aggregate type.
   *
   *  `FieldID`s are scoped by their enclosing `TypeID`.
   */
  trait FieldID

  /** ID of a function. */
  trait FunctionID

  /** ID of a local variable within a function (including parameters).
   *
   *  `LocalID`s are scoped by their enclosing `FunctionID`.
   */
  trait LocalID

  /** ID of a label within a function.
   *
   *  `LabelID`s are scoped by their enclosing `FunctionID`.
   */
  trait LabelID

  /** ID of an exception tag.
   *
   *  @see
   *    [[https://webassembly.github.io/exception-handling/core/syntax/modules.html#indices]]
   */
  trait TagID

  /** ID of a global variable. */
  trait GlobalID

  /** ID of a data segment. */
  trait DataID
}
