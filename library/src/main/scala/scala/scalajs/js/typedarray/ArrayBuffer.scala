package scala.scalajs.js.typedarray

import scala.scalajs.js

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  An ArrayBuffer is a block of contiguous, non-resizable memory.
 */
@js.native
class ArrayBuffer(length: Int) extends js.Object {

  /** Length of this buffer in bytes */
  val byteLength: Int = js.native

  /** Returns a copy of the given slice of this array buffer */
  def slice(begin: Int, end: Int = ???): ArrayBuffer = js.native

  // Note: Some specifications specify a static isView method on ArrayBuffer
  // that checks whether a given object is an ArrayBufferView. We omit it here
  // since neither Node.js nor PhantomJS support it at the time of writing.

}
