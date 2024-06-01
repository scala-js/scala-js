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
 *  The are encouraged to have a usable `toString()` implementation for debugging purposes.
 *
 *  See [[https://webassembly.github.io/gc/core/syntax/modules.html#indices]]
 */
object Identitities {
  trait LocalID

  trait LabelID

  trait GlobalID

  trait FunctionID

  trait FieldID

  trait TypeID

  trait TagID

  trait DataID
}
