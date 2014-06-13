/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


/**
 * All doc-comments marked as "MDN" are by Mozilla Contributors,
 * distributed under the Creative Commons Attribution-ShareAlike license from
 * https://developer.mozilla.org/en-US/docs/Web/Reference/API
 */
package scala.scalajs.js

import annotation.JSBracketAccess

import scala.language.{ dynamics, implicitConversions }
import scala.reflect.ClassTag
import scala.collection.{ immutable, mutable }

/** Super-type of all JavaScript values.
 *
 *  All values of a subtype of this trait represent JavaScript values, without
 *  boxing of proxying of any kind.
 */
trait Any extends scala.AnyRef {
  @deprecated("Considered abuse in typed JavaScript, will be removed in 0.6. "+
      "Use js.Dynamic or js.prim.Boolean instead.", "0.5.0")
  def unary_!(): Boolean = sys.error("stub")
}

/** Provides implicit conversions from Scala values to JavaScript values. */
object Any {
  implicit def fromUnit(value: Unit): prim.Undefined = sys.error("stub")

  implicit def fromBoolean(value: scala.Boolean): prim.Boolean = sys.error("stub")

  implicit def fromByte(value: scala.Byte): prim.Number = sys.error("stub")
  implicit def fromShort(value: scala.Short): prim.Number = sys.error("stub")
  implicit def fromInt(value: scala.Int): prim.Number = sys.error("stub")
  implicit def fromLong(value: scala.Long): prim.Number = value.toDouble
  implicit def fromFloat(value: scala.Float): prim.Number = sys.error("stub")
  implicit def fromDouble(value: scala.Double): prim.Number = sys.error("stub")

  implicit def fromString(s: java.lang.String): prim.String = sys.error("stub")

  implicit def fromArray[A](array: scala.Array[A]): Array[A] = {
    val length = array.length
    val result = new Array[A](length)
    var i = 0
    while (i < length) {
      result(i) = array(i)
      i += 1
    }
    result
  }

  implicit def toArray[A : ClassTag](array: Array[A]): scala.Array[A] = {
    val length = array.length.toInt
    val result = new scala.Array[A](length)
    var i = 0
    while (i < length) {
      result(i) = array(i)
      i += 1
    }
    result
  }

  def fromTraversableOnce[A](col: TraversableOnce[A]): Array[A] = {
    val result = new Array[A]
    col.foreach(x => result.push(x))
    result
  }

  implicit def arrayOps[A : ClassTag](array: Array[A]): mutable.ArrayOps[A] =
    genericArrayOps(toArray(array))
  implicit def stringOps(string: prim.String): immutable.StringOps =
    new immutable.StringOps(string: java.lang.String)
  implicit def richDouble(num: prim.Number): scala.runtime.RichDouble =
    new scala.runtime.RichDouble(num: scala.Double)
  implicit def richBoolean(b: prim.Boolean): scala.runtime.RichBoolean =
    new scala.runtime.RichBoolean(b: scala.Boolean)

  implicit def fromFunction0[R](f: scala.Function0[R]): Function0[R] = sys.error("stub")
  implicit def fromFunction1[T1, R](f: scala.Function1[T1, R]): Function1[T1, R] = sys.error("stub")
  implicit def fromFunction2[T1, T2, R](f: scala.Function2[T1, T2, R]): Function2[T1, T2, R] = sys.error("stub")
  implicit def fromFunction3[T1, T2, T3, R](f: scala.Function3[T1, T2, T3, R]): Function3[T1, T2, T3, R] = sys.error("stub")
  implicit def fromFunction4[T1, T2, T3, T4, R](f: scala.Function4[T1, T2, T3, T4, R]): Function4[T1, T2, T3, T4, R] = sys.error("stub")
  implicit def fromFunction5[T1, T2, T3, T4, T5, R](f: scala.Function5[T1, T2, T3, T4, T5, R]): Function5[T1, T2, T3, T4, T5, R] = sys.error("stub")
  implicit def fromFunction6[T1, T2, T3, T4, T5, T6, R](f: scala.Function6[T1, T2, T3, T4, T5, T6, R]): Function6[T1, T2, T3, T4, T5, T6, R] = sys.error("stub")
  implicit def fromFunction7[T1, T2, T3, T4, T5, T6, T7, R](f: scala.Function7[T1, T2, T3, T4, T5, T6, T7, R]): Function7[T1, T2, T3, T4, T5, T6, T7, R] = sys.error("stub")
  implicit def fromFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R](f: scala.Function8[T1, T2, T3, T4, T5, T6, T7, T8, R]): Function8[T1, T2, T3, T4, T5, T6, T7, T8, R] = sys.error("stub")
  implicit def fromFunction9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](f: scala.Function9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]): Function9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R] = sys.error("stub")
  implicit def fromFunction10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](f: scala.Function10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]): Function10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R] = sys.error("stub")
  implicit def fromFunction11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](f: scala.Function11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]): Function11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R] = sys.error("stub")
  implicit def fromFunction12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](f: scala.Function12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]): Function12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R] = sys.error("stub")
  implicit def fromFunction13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](f: scala.Function13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]): Function13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R] = sys.error("stub")
  implicit def fromFunction14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](f: scala.Function14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]): Function14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R] = sys.error("stub")
  implicit def fromFunction15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](f: scala.Function15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]): Function15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R] = sys.error("stub")
  implicit def fromFunction16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](f: scala.Function16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]): Function16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R] = sys.error("stub")
  implicit def fromFunction17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](f: scala.Function17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]): Function17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R] = sys.error("stub")
  implicit def fromFunction18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](f: scala.Function18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]): Function18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R] = sys.error("stub")
  implicit def fromFunction19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](f: scala.Function19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]): Function19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R] = sys.error("stub")
  implicit def fromFunction20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](f: scala.Function20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]): Function20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R] = sys.error("stub")
  implicit def fromFunction21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](f: scala.Function21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]): Function21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R] = sys.error("stub")
  implicit def fromFunction22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](f: scala.Function22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]): Function22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R] = sys.error("stub")

  implicit def toFunction0[R](f: Function0[R]): scala.Function0[R] = () => f()
  implicit def toFunction1[T1, R](f: Function1[T1, R]): scala.Function1[T1, R] = (x1) => f(x1)
  implicit def toFunction2[T1, T2, R](f: Function2[T1, T2, R]): scala.Function2[T1, T2, R] = (x1, x2) => f(x1, x2)
  implicit def toFunction3[T1, T2, T3, R](f: Function3[T1, T2, T3, R]): scala.Function3[T1, T2, T3, R] = (x1, x2, x3) => f(x1, x2, x3)
  implicit def toFunction4[T1, T2, T3, T4, R](f: Function4[T1, T2, T3, T4, R]): scala.Function4[T1, T2, T3, T4, R] = (x1, x2, x3, x4) => f(x1, x2, x3, x4)
  implicit def toFunction5[T1, T2, T3, T4, T5, R](f: Function5[T1, T2, T3, T4, T5, R]): scala.Function5[T1, T2, T3, T4, T5, R] = (x1, x2, x3, x4, x5) => f(x1, x2, x3, x4, x5)
  implicit def toFunction6[T1, T2, T3, T4, T5, T6, R](f: Function6[T1, T2, T3, T4, T5, T6, R]): scala.Function6[T1, T2, T3, T4, T5, T6, R] = (x1, x2, x3, x4, x5, x6) => f(x1, x2, x3, x4, x5, x6)
  implicit def toFunction7[T1, T2, T3, T4, T5, T6, T7, R](f: Function7[T1, T2, T3, T4, T5, T6, T7, R]): scala.Function7[T1, T2, T3, T4, T5, T6, T7, R] = (x1, x2, x3, x4, x5, x6, x7) => f(x1, x2, x3, x4, x5, x6, x7)
  implicit def toFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R](f: Function8[T1, T2, T3, T4, T5, T6, T7, T8, R]): scala.Function8[T1, T2, T3, T4, T5, T6, T7, T8, R] = (x1, x2, x3, x4, x5, x6, x7, x8) => f(x1, x2, x3, x4, x5, x6, x7, x8)
  implicit def toFunction9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](f: Function9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]): scala.Function9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9)
  implicit def toFunction10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](f: Function10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]): scala.Function10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  implicit def toFunction11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](f: Function11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]): scala.Function11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  implicit def toFunction12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](f: Function12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]): scala.Function12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
  implicit def toFunction13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](f: Function13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]): scala.Function13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)
  implicit def toFunction14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](f: Function14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]): scala.Function14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)
  implicit def toFunction15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](f: Function15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]): scala.Function15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
  implicit def toFunction16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](f: Function16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]): scala.Function16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16)
  implicit def toFunction17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](f: Function17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]): scala.Function17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17)
  implicit def toFunction18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](f: Function18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]): scala.Function18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18)
  implicit def toFunction19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](f: Function19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]): scala.Function19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)
  implicit def toFunction20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](f: Function20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]): scala.Function20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20)
  implicit def toFunction21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](f: Function21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]): scala.Function21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21)
  implicit def toFunction22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](f: Function22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]): scala.Function22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22)
}

/** Dynamically typed JavaScript value.
 *
 *  Values of this trait accept all possible JavaScript operations in a
 *  dynamically typed way. You can read and write any field, call any method,
 *  apply any JavaScript operator to values of this type.
 */
sealed trait Dynamic extends Any with scala.Dynamic {
  /** Calls a method of this object. */
  def applyDynamic(name: java.lang.String)(args: Any*): Dynamic

  /** Reads a field of this object. */
  def selectDynamic(name: java.lang.String): Dynamic

  /** Writes a field of this object. */
  def updateDynamic(name: java.lang.String)(value: Any): Unit

  /** Calls this object as a callable. */
  def apply(args: Any*): Dynamic

  import prim.Number

  override def unary_!(): Boolean = sys.error("stub")

  def unary_+(): Number
  def unary_-(): Number
  def unary_~(): Number

  def +(that: Number): Dynamic // could be a String if this is a String
  def -(that: Number): Number
  def *(that: Number): Number
  def /(that: Number): Number
  def %(that: Number): Number
  def <<(that: Number): Number
  def >>(that: Number): Number
  def >>>(that: Number): Number
  def &(that: Number): Number
  def |(that: Number): Number
  def ^(that: Number): Number

  def +(that: Dynamic): Dynamic // could be String if this or that is a String
  def -(that: Dynamic): Number
  def *(that: Dynamic): Number
  def /(that: Dynamic): Number
  def %(that: Dynamic): Number
  def <<(that: Dynamic): Number
  def >>(that: Dynamic): Number
  def >>>(that: Dynamic): Number
  def &(that: Dynamic): Number
  def |(that: Dynamic): Number
  def ^(that: Dynamic): Number

  def <(that: Number): Boolean
  def >(that: Number): Boolean
  def <=(that: Number): Boolean
  def >=(that: Number): Boolean

  def <(that: String): Boolean
  def >(that: String): Boolean
  def <=(that: String): Boolean
  def >=(that: String): Boolean

  def <(that: Dynamic): Boolean
  def >(that: Dynamic): Boolean
  def <=(that: Dynamic): Boolean
  def >=(that: Dynamic): Boolean

  /* The result of (dyn && bool) and (dyn || bool) has, in theory, type
   * (Dynamic v Boolean). This type cannot be expressed in Scala, but if it
   * could, the operations one could apply on a (Dynamic v Boolean) would be
   * the *intersection* of the operations one can apply on a Dynamic and on a
   * Boolean. Since any operation can be applied on a Dynamic, this
   * intersection is equal to the set of operations supported by Boolean.
   * Hence the result type is restricted to Boolean.
   */
  def &&(that: Boolean): Boolean
  def ||(that: Boolean): Boolean

  def &&(that: Dynamic): Dynamic
  def ||(that: Dynamic): Dynamic

  // Work around the annoying implicits in Predef in Scala 2.10.
  def x: Dynamic
  def x_=(value: Any): Unit
}

/** Factory for dynamically typed JavaScript values. */
object Dynamic {
  /** Dynamic view of the global scope. */
  def global: Dynamic = sys.error("stub")

  /** Instantiates a new object of a JavaScript class. */
  def newInstance(clazz: Dynamic)(args: Any*): Dynamic = sys.error("stub")

  /** Creates a new object with a literal syntax.
   *
   *  For example,
   *    js.Dynamic.literal(foo = 3, bar = "foobar")
   *  returns the JavaScript object
   *    {foo: 3, bar: "foobar"}
   */
  object literal extends scala.Dynamic {
    /** literal creation like this:
     *  js.Dynamic.literal(name1 = "value", name2 = "value")
     */
    def applyDynamicNamed(name: java.lang.String)(
        fields: (java.lang.String, Any)*): Dynamic = sys.error("stub")

    /** literal creation like this:
     *  js.Dynamic.literal("name1" -> "value", "name2" -> "value")
     *
     *  Note that this could be simply `def apply`, but this would make the
     *  applyDynamicNamed fail, since a call with named arguments would
     *  be routed to the `def apply`, rather than def dynamic version.
     */
    def applyDynamic(name: java.lang.String)(
        fields: (java.lang.String, Any)*): Dynamic = sys.error("stub")

  }
}

/** Base class of all JavaScript objects. */
class Object extends Any {
  def this(value: Any) = this()

  def toLocaleString(): String = ???
  def valueOf(): scala.Any = ???

  /** Tests whether this object has the specified property as a direct property.
   *
   *  Unlike [[js.Object.hasProperty]], this method does not check down the
   *  object's prototype chain.
   *
   * MDN
   */
  def hasOwnProperty(v: String): Boolean = ???

  /** Tests whether this object is in the prototype chain of another object. */
  def isPrototypeOf(v: Object): Boolean = ???

  /** Tests whether the specified property in an object can be enumerated by a
   *  call to [[js.Object.properties]], with the exception of properties
   *  inherited through the prototype chain. If the object does not have the
   *  specified property, this method returns false.
   *
   *  MDN
   */
  def propertyIsEnumerable(v: String): Boolean = ???
}

/** The top-level `Object` JavaScript object. */
object Object extends Object {
  def apply(): Object = ???
  def apply(value: Any): Object = ???

  /** Tests whether the object has a property on itself or in its prototype
   *  chain. This method is the equivalent of `p in o` in JavaScript.
   */
  def hasProperty(o: Object, p: String): Boolean = sys.error("stub")

  /**
   * The Object.getPrototypeOf() method returns the prototype (i.e. the
   * internal [[Prototype]]) of the specified object.
   *
   * MDN
   */
  def getPrototypeOf(o: Object): Any = ???

  /**
   * The Object.getOwnPropertyDescriptor() method returns a property descriptor
   * for an own property (that is, one directly present on an object, not
   * present by dint of being along an object's prototype chain) of a given object.
   *
   * MDN
   */
  def getOwnPropertyDescriptor(o: Object, p: String): PropertyDescriptor = ???

  /**
   * Object.getOwnPropertyNames returns an array whose elements are strings
   * corresponding to the enumerable and non-enumerable properties found
   * directly upon obj. The ordering of the enumerable properties in the array
   * is consistent with the ordering exposed by a for...in loop (or by Object.keys)
   * over the properties of the object. The ordering of the non-enumerable
   * properties in the array, and among the enumerable properties, is not defined.
   *
   * MDN
   */
  def getOwnPropertyNames(o: Object): Array[String] = ???

  /**
   * The Object.create() method creates a new object with the specified
   * prototype object and properties.
   *
   * MDN
   */
  def create(o: Object, properties: Any): Object = ???
  def create(o: Object): Object = ???

  /**
   * The Object.defineProperty() method defines a new property directly on an
   * object, or modifies an existing property on an object, and returns the
   * object.
   *
   * This method allows precise addition to or modification of a property on an
   * object. Normal property addition through assignment creates properties
   * which show up during property enumeration (for...in loop or Object.keys method),
   * whose values may be changed, and which may be deleted. This method allows
   * these extra details to be changed from their defaults.
   *
   * Property descriptors present in objects come in two main flavors: data
   * descriptors and accessor descriptors. A data descriptor is a property
   * that has a value, which may or may not be writable. An accessor descriptor
   * is a property described by a getter-setter pair of functions. A descriptor
   * must be one of these two flavors; it cannot be both.
   *
   * MDN
   */
  def defineProperty(o: Object, p: String, attributes: PropertyDescriptor): o.type = ???

  /**
   * The Object.defineProperties() method defines new or modifies existing
   * properties directly on an object, returning the object.
   *
   * MDN
   */
  def defineProperties(o: Object, properties: Any): o.type = ???

  /**
   * The Object.seal() method seals an object, preventing new properties from
   * being added to it and marking all existing properties as non-configurable.
   * Values of present properties can still be changed as long as they are
   * writable.
   *
   * MDN
   */
  def seal(o: Object): o.type = ???

  /**
   * The Object.freeze() method freezes an object: that is, prevents new properties
   * from being added to it; prevents existing properties from being removed;
   * and prevents existing properties, or their enumerability, configurability,
   * or writability, from being changed. In essence the object is made effectively
   * immutable. The method returns the object being frozen.
   *
   * MDN
   */
  def freeze(o: Object): o.type = ???

  /**
   * The Object.preventExtensions() method prevents new properties from ever
   * being added to an object (i.e. prevents future extensions to the object).
   *
   * An object is extensible if new properties can be added to it.  preventExtensions
   * marks an object as no longer extensible, so that it will never have
   * properties beyond the ones it had at the time it was marked as non-extensible.
   * Note that the properties of a non-extensible object, in general, may still be
   * deleted. Attempting to add new properties to a non-extensible object will
   * fail, either silently or by throwing a TypeError (most commonly, but not
   * exclusively, when in strict mode).
   *
   * Object.preventExtensions only prevents addition of own properties. Properties
   * can still be added to the object prototype. However, calling Object.preventExtensions
   * on an object will also prevent extensions on its __proto__ property.
   *
   * MDN
   */
  def preventExtensions(o: Object): o.type = ???

  /**
   * Returns true if the object is sealed, otherwise false. An object is sealed
   * if it is not extensible and if all its properties are non-configurable and
   * therefore not removable (but not necessarily non-writable).
   *
   * MDN
   */
  def isSealed(o: Object): Boolean = ???

  /**
   * The Object.isFrozen() determines if an object is frozen.
   *
   * An object is frozen if and only if it is not extensible, all its properties
   * are non-configurable, and all its data properties (that is, properties which
   * are not accessor properties with getter or setter components) are non-writable.
   *
   * MDN
   */
  def isFrozen(o: Object): Boolean = ???

  /**
   * Determines if extending of an object is allowed
   *
   * Objects are extensible by default: they can have new properties added to
   * them, and (in engines that support __proto__  their __proto__ property)
   * can be modified. An object can be marked as non-extensible using
   * Object.preventExtensions, Object.seal, or Object.freeze
   *
   * MDN
   */
  def isExtensible(o: Object): Boolean = ???

  /**
   * The Object.keys() method returns an array of a given object's own enumerable
   * properties, in the same order as that provided by a for...in loop (the
   * difference being that a for-in loop enumerates properties in the prototype
   * chain as well).
   *
   * MDN
   */
  def keys(o: Object): Array[String] = ???

  /** Returns the names of all the enumerable properties of this object.
   *  This is the equivalent of a for...in loop in JavaScript.
   */
  def properties(o: Object): Array[String] = sys.error("stub")
}
