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

package scala.scalajs.js.typedarray

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  An ArrayBuffer is a block of contiguous, non-resizable memory.
 */
@js.native
@JSGlobal
class ArrayBuffer(length: Int) extends js.Object {

  /** Length of this buffer in bytes */
  val byteLength: Int = js.native

  /** Returns a copy of the given slice of this array buffer */
  def slice(begin: Int, end: Int = ???): ArrayBuffer = js.native

  // Note: Some specifications specify a static isView method on ArrayBuffer
  // that checks whether a given object is an ArrayBufferView. We omit it here
  // since neither Node.js nor PhantomJS support it at the time of writing.

}
