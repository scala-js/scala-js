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
 *  A [[TypedArray]] of unsigned 16-bit integers
 */
@js.native
@JSGlobal
class Uint16Array private[this] () extends TypedArray[Int, Uint16Array] {

  /** Constructs a Uint16Array with the given length. Initialized to all 0 */
  def this(length: Int) = this()

  /** Creates a new Uint16Array with the same elements than the given TypedArray
   *
   *  The elements are converted before being stored in the new Int8Array.
   */
  def this(typedArray: Uint16Array) = this()

  /** Creates a new Uint16Array with the elements in the given array */
  def this(array: js.Iterable[Int]) = this()

  /** Creates a Uint16Array view on the given ArrayBuffer */
  def this(buffer: ArrayBuffer, byteOffset: Int = 0, length: Int = ???) = this()

}

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  [[Uint16Array]] companion
 */
@js.native
@JSGlobal
object Uint16Array extends TypedArrayStatic[Int, Uint16Array]
