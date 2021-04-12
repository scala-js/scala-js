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

/** <span class="badge badge-ecma2020" style="float: right;">ECMAScript 2020</span>
 *
 *  A [[TypedArray]] of unsigned 64-bit integers represented as [[js.BigInt]].
 */
@js.native
@JSGlobal
class BigUint64Array private[this] ()
    extends TypedArray[js.BigInt, BigUint64Array] {

  /** Constructs a BigUint64Array with the given length. Initialized to all 0 */
  def this(length: Int) = this()

  /** Creates a new BigInt64Array with the same elements than the given TypedArray
   *
   *  Each elements must be BigInt (no conversion).
   */
  def this(typedArray: BigUint64Array) = this()

  /** Creates a new BigInt64Array with the elements in the given array.
   *
   *  Each elements must be BigInt (no conversion).
   */
  def this(array: js.Iterable[js.BigInt]) = this()

  /** Creates a BigInt64Array view on the given ArrayBuffer */
  def this(buffer: ArrayBuffer, byteOffset: Int = 0, length: Int = js.native) = this()

}

/** <span class="badge badge-ecma2020" style="float: right;">ECMAScript 2020</span>
 *  [[BigUint64Array]] companion
 */
@js.native
@JSGlobal
object BigUint64Array extends TypedArrayStatic[js.BigInt, BigUint64Array]
