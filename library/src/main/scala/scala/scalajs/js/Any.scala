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

import scala.language.implicitConversions

import scala.collection.mutable
import scala.collection.generic.CanBuildFrom

import annotation.ScalaJSDefined

/** Root of the hierarchy of JavaScript types.
 *
 *  Subtypes of [[Any js.Any]] are JavaScript types, which have different
 *  semantics and guarantees than Scala types (subtypes of [[AnyRef]] and
 *  [[AnyVal]]). Operations on JavaScript types behave as the corresponding
 *  operations in the JavaScript language.
 *
 *  By default, JavaScript types are native: they are facade types to APIs
 *  implemented in JavaScript code. Their implementation is irrelevant and
 *  never emitted. As such, all members must be defined with their
 *  right-hand-side being [[native js.native]]. For forward source
 *  compatibility with the next major version, the class/trait/object itself
 *  should be annotated with [[native @js.native]].
 *
 *  In most cases, you should not directly extend this trait, but rather extend
 *  [[Object js.Object]].
 *
 *  To implement a JavaScript type in Scala.js (therefore non-native), its
 *  declaration must be annotated with
 *  [[annotation.ScalaJSDefined @ScalaJSDefined]]. Scala.js-defined JS types
 *  cannot directly extend native JS traits; and Scala.js-defined JS traits
 *  cannot declare concrete term members.
 *
 *  It is not possible to define traits or classes that inherit both from this
 *  trait and a strict subtype of [[AnyRef]]. In fact, you should think of
 *  [[Any js.Any]] as a third direct subclass of [[scala.Any]], besides
 *  [[scala.AnyRef]] and [[scala.AnyVal]].
 *
 *  See the [[http://www.scala-js.org/doc/js-interoperability.html JavaScript
 *  interoperability guide]] of Scala.js for more details.
 */
@ScalaJSDefined
trait Any extends scala.AnyRef

/** Provides implicit conversions from Scala values to JavaScript values. */
object Any extends LowPrioAnyImplicits {
  @inline implicit def fromUnit(value: Unit): Any =
    value.asInstanceOf[Any]
  @inline implicit def fromBoolean(value: Boolean): Any =
    value.asInstanceOf[Any]
  @inline implicit def fromByte(value: Byte): Any =
    value.asInstanceOf[Any]
  @inline implicit def fromShort(value: Short): Any =
    value.asInstanceOf[Any]
  @inline implicit def fromInt(value: Int): Any =
    value.asInstanceOf[Any]
  @inline implicit def fromLong(value: Long): Any =
    value.toDouble.asInstanceOf[Any]
  @inline implicit def fromFloat(value: Float): Any =
    value.asInstanceOf[Any]
  @inline implicit def fromDouble(value: Double): Any =
    value.asInstanceOf[Any]
  @inline implicit def fromString(s: String): Any =
    s.asInstanceOf[Any]

  implicit def jsArrayOps[A](array: Array[A]): ArrayOps[A] =
    new ArrayOps(array)

  implicit def canBuildFromArray[A]: CanBuildFrom[Array[_], A, Array[A]] = {
    @inline
    class CanBuildFromArray extends CanBuildFrom[Array[_], A, Array[A]] {
      def apply(from: Array[_]): mutable.Builder[A, Array[A]] =
        new ArrayOps[A]
      def apply(): mutable.Builder[A, Array[A]] =
        new ArrayOps[A]
    }
    new CanBuildFromArray
  }

  // scalastyle:off line.size.limit
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
  // scalastyle:on line.size.limit

  @inline implicit def fromJBoolean(value: java.lang.Boolean): Any =
    value.asInstanceOf[Any]
  @inline implicit def fromJByte(value: java.lang.Byte): Any =
    value.asInstanceOf[Any]
  @inline implicit def fromJShort(value: java.lang.Short): Any =
    value.asInstanceOf[Any]
  @inline implicit def fromJInteger(value: java.lang.Integer): Any =
    value.asInstanceOf[Any]

  @inline implicit def fromJLong(value: java.lang.Long): Any =
    if (value eq null) null
    else value.doubleValue.asInstanceOf[Any]

  @inline implicit def fromJFloat(value: java.lang.Float): Any =
    value.asInstanceOf[Any]
  @inline implicit def fromJDouble(value: java.lang.Double): Any =
    value.asInstanceOf[Any]
}

trait LowPrioAnyImplicits {
  implicit def wrapArray[A](array: Array[A]): WrappedArray[A] =
    new WrappedArray(array)
  implicit def wrapDictionary[A](dict: Dictionary[A]): WrappedDictionary[A] =
    new WrappedDictionary(dict)
}
