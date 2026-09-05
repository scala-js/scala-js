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

package scala.scalajs.wasm.annotation

/** Marks the annotated method as a Wasm exported function.
 *
 *  Loosely speaking, `@WasmExport("name") def method(...)` corresponds
 *  to the following Wasm declaration:
 *
 *  {{{
 *  (func \$method (export "name") ...)
 *  }}}
 *
 *  Parameters and result types must be part of the following table, with the
 *  corresponding Wasm types.
 *
 *  {{{
 *  Int            |  i32
 *  Long           |  i64
 *  Float          |  f32
 *  Double         |  f64
 *  Array[Byte]    |  (ref (array (mut i8)))
 *  Array[Short]   |  (ref (array (mut i16)))
 *  Array[Int]     |  (ref (array (mut i32)))
 *  Array[Long]    |  (ref (array (mut i64)))
 *  Array[Float]   |  (ref (array (mut f32)))
 *  Array[Double]  |  (ref (array (mut f64)))
 *  Unit           |  [] (only as result type)
 *  }}}
 *
 *  Arrays are always copied at the boundary. They are never passed by reference.
 *
 *  The annotated method must be static. A definition is static if it is
 *  top-level or declared in an `object` that is itself static.
 */
class WasmExport private () extends scala.annotation.StaticAnnotation {
  def this(exportName: String) = this()
}
