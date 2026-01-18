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

package scala.scalajs.js

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** <span class="badge badge-ecma2020" style="float: right;">ECMAScript 2020</span>
 *
 *  A built-in object that provides a way to represent whole numbers larger than
 *  2 ^ 53 - 1, which is the largest number JavaScript can reliably represent
 *  with the Number primitive.
 *
 *  BigInt can be used for arbitrarily large integers.
 *
 *  @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
 */
@js.native
@JSGlobal
final class BigInt private[this] () extends js.Object {

  @JSOperator def +(other: BigInt): BigInt = js.native
  @JSOperator def *(other: BigInt): BigInt = js.native
  @JSOperator def /(other: BigInt): BigInt = js.native
  @JSOperator def -(other: BigInt): BigInt = js.native
  @JSOperator def %(other: BigInt): BigInt = js.native
  @JSOperator def **(other: BigInt): BigInt = js.native

  @JSOperator def &(other: BigInt): BigInt = js.native
  @JSOperator def |(other: BigInt): BigInt = js.native
  @JSOperator def ^(other: BigInt): BigInt = js.native
  @JSOperator def <<(other: BigInt): BigInt = js.native
  @JSOperator def >>(other: BigInt): BigInt = js.native
  // no >>> since BigInt is always signed

  @JSOperator def unary_- : BigInt = js.native
  @JSOperator def unary_~ : BigInt = js.native
  // unary_+ is not supported by BigInts

  @JSOperator def <(x: BigInt): Boolean = js.native
  @JSOperator def <=(x: BigInt): Boolean = js.native
  @JSOperator def >(x: BigInt): Boolean = js.native
  @JSOperator def >=(x: BigInt): Boolean = js.native

  /** Returns a localized string representation of this BigInt.
   *
   *  @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt/toLocaleString
   *  @param locale A string with a BCP 47 language tag, or an array of such
   *                strings. For the general form and interpretation of the
   *                locales argument, see the Intl page.
   *  @param options An object with some or all of the properties.
   */
  def toLocaleString(locale: String,
      options: BigInt.ToLocaleStringOptions = js.native): String = js.native

  /** Returns a string representation of this BigInt.
   *
   *  The trailing "n" is not part of the string.
   *
   *  @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt/toString
   */
  override def toString(): String = js.native

  /** Returns a string representation of this BigInt.
   *
   *  The trailing "n" is not part of the string.
   *
   *  @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt/toString
   */
  def toString(radix: Int): String = js.native
}

/** <span class="badge badge-ecma2020" style="float: right;">ECMAScript 2020</span>
 *
 *  A companion object of BigInt class.
 *
 *  @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
 */
@js.native
@JSGlobal
object BigInt extends js.Object {

  /** Returns a BigInt from a whole Double.
   *
   *  This overload is exposed to allow users to create a BigInt from a Double
   *  which is greater than Int.MaxValue.
   *
   *  @throws RangeError if value has a fractional portion
   *  @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
   */
  def apply(value: Double): BigInt = js.native

  /** @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt */
  def apply(value: Int): BigInt = js.native

  /** @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt */
  def apply(value: String): BigInt = js.native

  /** @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt/asIntN */
  def asIntN(width: Int, bigint: BigInt): BigInt = js.native

  /** @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt/asUintN */
  def asUintN(width: Int, bigint: BigInt): BigInt = js.native

  /** Type of the `options` parameter of [[BigInt.toLocaleString]].
   *
   *  @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt/toLocaleString#Parameters
   */
  trait ToLocaleStringOptions extends Object {
    var localeMatcher: js.UndefOr[String] = js.undefined
    var style: js.UndefOr[String] = js.undefined
    var currency: js.UndefOr[String] = js.undefined
    var currencyDisplay: js.UndefOr[String] = js.undefined
    var useGrouping: js.UndefOr[Boolean] = js.undefined
    var minimumIntegerDigits: js.UndefOr[Int] = js.undefined
    var minimumFractionDigits: js.UndefOr[Int] = js.undefined
    var maximumFractionDigits: js.UndefOr[Int] = js.undefined
    var minimumSignificantDigits: js.UndefOr[Int] = js.undefined
    var maximumSignificantDigits: js.UndefOr[Int] = js.undefined
  }
}
