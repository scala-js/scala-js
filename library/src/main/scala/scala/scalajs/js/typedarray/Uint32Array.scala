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
 *  A [[TypedArray]] of unsigned 32-bit integers
 */
@js.native
@JSGlobal
class Uint32Array private[this] () extends TypedArray[Double, Uint32Array] {

  /** Constructs a Uint32Array with the given length. Initialized to all 0 */
  def this(length: Int) = this()

  /** Creates a new Uint32Array with the same elements than the given TypedArray
   *
   *  The elements are converted before being stored in the new Int8Array.
   */
  def this(typedArray: Uint32Array) = this()

  /** Creates a new Uint32Array with the elements in the given array */
  def this(array: js.Iterable[Double]) = this()

  /** Creates a Uint32Array view on the given ArrayBuffer */
  def this(buffer: ArrayBuffer, byteOffset: Int = 0, length: Int = ???) = this()

}

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  [[Uint32Array]] companion
 */
@js.native
@JSGlobal
object Uint32Array extends TypedArrayStatic[Double, Uint32Array]
