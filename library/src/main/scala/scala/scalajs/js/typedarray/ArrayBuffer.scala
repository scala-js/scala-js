package scala.scalajs.js.typedarray

import scala.scalajs.js

class ArrayBuffer(length: Int) extends js.Object {

  /** Length of this buffer in bytes */
  val byteLength: Int = ???

  /** Returns a copy of the given slice of this array buffer */
  def slice(begin: Int, end: Int = ???): ArrayBuffer = ???

  // Note: Some specifications specify a static isView method on ArrayBuffer
  // that checks whether a given object is an ArrayBufferView. We omit it here
  // since neither Node.js nor PhantomJS support it at the time of writing.

}
