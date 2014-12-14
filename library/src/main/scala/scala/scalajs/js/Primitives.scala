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
import scala.collection.generic.CanBuildFrom

/** Supertype of all JavaScript values.
 *
 *  Subtypes of [[Any js.Any]] are facade types to APIs implemented in
 *  JavaScript code. Their implementation is irrelevant and never emitted.
 *  As such, all members must be defined with their right-hand-side being
 *  [[native js.native]].
 *
 *  In most cases, you should extend [[Object js.Object]] instead of this
 *  trait to define facade types.
 *
 *  It is not possible to define traits or classes that inherit both from this
 *  trait and a strict subtype of [[AnyRef]]. In fact, you should think of
 *  [[Any js.Any]] as a third direct subclass of [[scala.Any]], besides
 *  [[scala.AnyRef]] and [[scala.AnyVal]].
 *
 *  See the [[http://www.scala-js.org/doc/js-interoperability.html JavaScript
 *  interoperability guide]] of Scala.js for more details.
 */
trait Any extends scala.AnyRef

/** Provides implicit conversions from Scala values to JavaScript values. */
object Any extends LowPrioAnyImplicits {
  @inline implicit def fromUnit(value: Unit): prim.Undefined =
    value.asInstanceOf[prim.Undefined]
  @inline implicit def fromBoolean(value: scala.Boolean): prim.Boolean =
    value.asInstanceOf[prim.Boolean]
  @inline implicit def fromByte(value: scala.Byte): prim.Number =
    value.asInstanceOf[prim.Number]
  @inline implicit def fromShort(value: scala.Short): prim.Number =
    value.asInstanceOf[prim.Number]
  @inline implicit def fromInt(value: scala.Int): prim.Number =
    value.asInstanceOf[prim.Number]
  @inline implicit def fromLong(value: scala.Long): prim.Number =
    value.toDouble.asInstanceOf[prim.Number]
  @inline implicit def fromFloat(value: scala.Float): prim.Number =
    value.asInstanceOf[prim.Number]
  @inline implicit def fromDouble(value: scala.Double): prim.Number =
    value.asInstanceOf[prim.Number]
  @inline implicit def fromString(s: java.lang.String): prim.String =
    s.asInstanceOf[prim.String]

  @inline implicit def toDouble(value: prim.Number): scala.Double =
    value.asInstanceOf[scala.Double]
  @inline implicit def toBoolean(value: prim.Boolean): scala.Boolean =
    value.asInstanceOf[scala.Boolean]
  @inline implicit def toScalaString(value: prim.String): java.lang.String =
    value.asInstanceOf[java.lang.String]

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

trait LowPrioAnyImplicits {
  @inline implicit def richDouble(num: prim.Number): scala.runtime.RichDouble =
    new scala.runtime.RichDouble(Any.toDouble(num))
  @inline implicit def richBoolean(b: prim.Boolean): scala.runtime.RichBoolean =
    new scala.runtime.RichBoolean(Any.toBoolean(b))

  @inline implicit def stringOps(string: prim.String): immutable.StringOps =
    new immutable.StringOps(Any.toScalaString(string))

  implicit def wrapArray[A](array: Array[A]): WrappedArray[A] =
    new WrappedArray(array)
  implicit def wrapDictionary[A](dict: Dictionary[A]): WrappedDictionary[A] =
    new WrappedDictionary(dict)
}

/** Dynamically typed JavaScript value.
 *
 *  Values of this trait accept all possible JavaScript operations in a
 *  dynamically typed way. You can read and write any field, call any method,
 *  apply any JavaScript operator to values of this type.
 */
sealed trait Dynamic extends Any with scala.Dynamic {
  /** Calls a method of this object. */
  def applyDynamic(name: java.lang.String)(args: Any*): Dynamic = sys.error("stub")

  /** Reads a field of this object. */
  @JSBracketAccess
  def selectDynamic(name: java.lang.String): Dynamic = native

  /** Writes a field of this object. */
  @JSBracketAccess
  def updateDynamic(name: java.lang.String)(value: Any): Unit = native

  /** Calls this object as a callable. */
  def apply(args: Any*): Dynamic = native

  import prim.Number

  def unary_!(): Boolean = native

  def unary_+(): Number = native
  def unary_-(): Number = native
  def unary_~(): Number = native

  def +(that: Number): Dynamic = native // could be a String if this is a String
  def -(that: Number): Number = native
  def *(that: Number): Number = native
  def /(that: Number): Number = native
  def %(that: Number): Number = native
  def <<(that: Number): Number = native
  def >>(that: Number): Number = native
  def >>>(that: Number): Number = native
  def &(that: Number): Number = native
  def |(that: Number): Number = native
  def ^(that: Number): Number = native

  def +(that: Dynamic): Dynamic = native // could be String if this or that is a String
  def -(that: Dynamic): Number = native
  def *(that: Dynamic): Number = native
  def /(that: Dynamic): Number = native
  def %(that: Dynamic): Number = native
  def <<(that: Dynamic): Number = native
  def >>(that: Dynamic): Number = native
  def >>>(that: Dynamic): Number = native
  def &(that: Dynamic): Number = native
  def |(that: Dynamic): Number = native
  def ^(that: Dynamic): Number = native

  def <(that: Number): Boolean = native
  def >(that: Number): Boolean = native
  def <=(that: Number): Boolean = native
  def >=(that: Number): Boolean = native

  def <(that: String): Boolean = native
  def >(that: String): Boolean = native
  def <=(that: String): Boolean = native
  def >=(that: String): Boolean = native

  def <(that: Dynamic): Boolean = native
  def >(that: Dynamic): Boolean = native
  def <=(that: Dynamic): Boolean = native
  def >=(that: Dynamic): Boolean = native

  /* The result of (dyn && bool) and (dyn || bool) has, in theory, type
   * (Dynamic v Boolean). This type cannot be expressed in Scala, but if it
   * could, the operations one could apply on a (Dynamic v Boolean) would be
   * the *intersection* of the operations one can apply on a Dynamic and on a
   * Boolean. Since any operation can be applied on a Dynamic, this
   * intersection is equal to the set of operations supported by Boolean.
   * Hence the result type is restricted to Boolean.
   */
  def &&(that: Boolean): Boolean = native
  def ||(that: Boolean): Boolean = native

  def &&(that: Dynamic): Dynamic = native
  def ||(that: Dynamic): Dynamic = native

  // Work around the annoying implicits in Predef in Scala 2.10.
  def x: Dynamic = native
  def x_=(value: Any): Unit = native
}

/** Factory for dynamically typed JavaScript values. */
object Dynamic {
  /** Dynamic view of the global scope. */
  @inline def global: Dynamic = scala.scalajs.runtime.environmentInfo.global

  /** Instantiates a new object of a JavaScript class. */
  def newInstance(clazz: Dynamic)(args: Any*): Object with Dynamic = sys.error("stub")

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
        fields: (java.lang.String, Any)*): Object with Dynamic = sys.error("stub")

    /** literal creation like this:
     *  js.Dynamic.literal("name1" -> "value", "name2" -> "value")
     *
     *  Note that this could be simply `def apply`, but this would make the
     *  applyDynamicNamed fail, since a call with named arguments would
     *  be routed to the `def apply`, rather than def dynamic version.
     */
    def applyDynamic(name: java.lang.String)(
        fields: (java.lang.String, Any)*): Object with Dynamic = sys.error("stub")

  }
}

/** Base class of all JavaScript objects. */
class Object extends Any {
  def this(value: Any) = this()

  def toLocaleString(): String = native
  def valueOf(): scala.Any = native

  /** Tests whether this object has the specified property as a direct property.
   *
   *  Unlike [[js.Object.hasProperty]], this method does not check down the
   *  object's prototype chain.
   *
   * MDN
   */
  def hasOwnProperty(v: String): Boolean = native

  /** Tests whether this object is in the prototype chain of another object. */
  def isPrototypeOf(v: Object): Boolean = native

  /** Tests whether the specified property in an object can be enumerated by a
   *  call to [[js.Object.properties]], with the exception of properties
   *  inherited through the prototype chain. If the object does not have the
   *  specified property, this method returns false.
   *
   *  MDN
   */
  def propertyIsEnumerable(v: String): Boolean = native
}

/** The top-level `Object` JavaScript object. */
object Object extends Object {
  def apply(): Object = native
  def apply(value: Any): Object = native

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
  def getPrototypeOf(o: Object): Object = native

  /**
   * The Object.getOwnPropertyDescriptor() method returns a property descriptor
   * for an own property (that is, one directly present on an object, not
   * present by dint of being along an object's prototype chain) of a given object.
   *
   * MDN
   */
  def getOwnPropertyDescriptor(o: Object, p: String): PropertyDescriptor = native

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
  def getOwnPropertyNames(o: Object): Array[String] = native

  /**
   * The Object.create() method creates a new object with the specified
   * prototype object and properties.
   *
   * MDN
   */
  def create(o: Object, properties: Any): Object = native
  def create(o: Object): Object = native

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
  def defineProperty(o: Object, p: String, attributes: PropertyDescriptor): o.type = native

  /**
   * The Object.defineProperties() method defines new or modifies existing
   * properties directly on an object, returning the object.
   *
   * MDN
   */
  def defineProperties(o: Object, properties: Any): o.type = native

  /**
   * The Object.seal() method seals an object, preventing new properties from
   * being added to it and marking all existing properties as non-configurable.
   * Values of present properties can still be changed as long as they are
   * writable.
   *
   * MDN
   */
  def seal(o: Object): o.type = native

  /**
   * The Object.freeze() method freezes an object: that is, prevents new properties
   * from being added to it; prevents existing properties from being removed;
   * and prevents existing properties, or their enumerability, configurability,
   * or writability, from being changed. In essence the object is made effectively
   * immutable. The method returns the object being frozen.
   *
   * MDN
   */
  def freeze(o: Object): o.type = native

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
  def preventExtensions(o: Object): o.type = native

  /**
   * Returns true if the object is sealed, otherwise false. An object is sealed
   * if it is not extensible and if all its properties are non-configurable and
   * therefore not removable (but not necessarily non-writable).
   *
   * MDN
   */
  def isSealed(o: Object): Boolean = native

  /**
   * The Object.isFrozen() determines if an object is frozen.
   *
   * An object is frozen if and only if it is not extensible, all its properties
   * are non-configurable, and all its data properties (that is, properties which
   * are not accessor properties with getter or setter components) are non-writable.
   *
   * MDN
   */
  def isFrozen(o: Object): Boolean = native

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
  def isExtensible(o: Object): Boolean = native

  /**
   * The Object.keys() method returns an array of a given object's own enumerable
   * properties, in the same order as that provided by a for...in loop (the
   * difference being that a for-in loop enumerates properties in the prototype
   * chain as well).
   *
   * MDN
   */
  def keys(o: Object): Array[String] = native

  /** Returns the names of all the enumerable properties of this object,
   *  including properties in its prototype chain.
   *
   *  This method returns the same set of names that would be enumerated by
   *  a for-in loop in JavaScript, but not necessarily in the same order.
   *
   *  If the underlying implementation guarantees an order for for-in loops,
   *  then this is guaranteed to be consistent with [[keys]], in the sense
   *  that the list returned by [[keys]] is a sublist of the list returned by
   *  this method (not just a subset).
   */
  def properties(o: Any): Array[String] = sys.error("stub")
}

package prim {

/** Primitive JavaScript number.
 *
 *  In most situations, you should not need this trait, and use
 *  [[scala.Double]] instead (or [[scala.Int]] where appropriate).
 */
sealed trait Number extends Any {
  def unary_+(): Number = native
  def unary_-(): Number = native
  def unary_~(): Number = native

  def +(that: Number): Number = native
  def -(that: Number): Number = native
  def *(that: Number): Number = native
  def /(that: Number): Number = native
  def %(that: Number): Number = native
  def <<(that: Number): Number = native
  def >>(that: Number): Number = native
  def >>>(that: Number): Number = native
  def &(that: Number): Number = native
  def |(that: Number): Number = native
  def ^(that: Number): Number = native

  def +(that: Dynamic): Dynamic = native // could be a String if that is a String
  def -(that: Dynamic): Number = native
  def *(that: Dynamic): Number = native
  def /(that: Dynamic): Number = native
  def %(that: Dynamic): Number = native
  def <<(that: Dynamic): Number = native
  def >>(that: Dynamic): Number = native
  def >>>(that: Dynamic): Number = native
  def &(that: Dynamic): Number = native
  def |(that: Dynamic): Number = native
  def ^(that: Dynamic): Number = native

  def <(that: Number): Boolean = native
  def >(that: Number): Boolean = native
  def <=(that: Number): Boolean = native
  def >=(that: Number): Boolean = native

  def <(that: Dynamic): Boolean = native
  def >(that: Dynamic): Boolean = native
  def <=(that: Dynamic): Boolean = native
  def >=(that: Dynamic): Boolean = native

  def toString(radix: Number): String = native

  /**
   * Returns a string representation of number that does not use exponential
   * notation and has exactly digits digits after the decimal place. The number
   * is rounded if necessary, and the fractional part is padded with zeros if
   * necessary so that it has the specified length. If number is greater than
   * 1e+21, this method simply calls Number.prototype.toString() and returns
   * a string in exponential notation.
   *
   * MDN
   */
  def toFixed(fractionDigits: Number): String = native
  def toFixed(): String = native

  /**
   * Returns a string representing a Number object in exponential notation with one
   * digit before the decimal point, rounded to fractionDigits digits after the
   * decimal point. If the fractionDigits argument is omitted, the number of
   * digits after the decimal point defaults to the number of digits necessary
   * to represent the value uniquely.
   *
   * If a number has more digits that requested by the fractionDigits parameter,
   * the number is rounded to the nearest number represented by fractionDigits
   * digits. See the discussion of rounding in the description of the toFixed()
   * method, which also applies to toExponential().
   *
   * MDN
   */
  def toExponential(fractionDigits: Number): String = native
  def toExponential(): String = native

  /**
   * Returns a string representing a Number object in fixed-point or exponential
   * notation rounded to precision significant digits. See the discussion of
   * rounding in the description of the Number.prototype.toFixed() method, which
   * also applies to toPrecision.
   *
   * If the precision argument is omitted, behaves as Number.prototype.toString().
   * If it is a non-integer value, it is rounded to the nearest integer.
   *
   * MDN
   */
  def toPrecision(precision: Number): String = native
  def toPrecision(): String = native
}

/** The top-level `Number` JavaScript object */
object Number extends Object {
  /**
   * The Number.MAX_VALUE property represents the maximum numeric value
   * representable in JavaScript.
   *
   * The MAX_VALUE property has a value of approximately 1.79E+308. Values
   * larger than MAX_VALUE are represented as "Infinity".
   *
   * MDN
   */
  val MAX_VALUE: Double = native
  /**
   * The Number.MIN_VALUE property represents the smallest positive numeric
   * value representable in JavaScript.
   *
   * The MIN_VALUE property is the number closest to 0, not the most negative
   * number, that JavaScript can represent.
   *
   * MIN_VALUE has a value of approximately 5e-324. Values smaller than MIN_VALUE
   * ("underflow values") are converted to 0.
   *
   * MDN
   */
  val MIN_VALUE: Double = native
  /**
   * The Number.NaN property represents Not-A-Number. Equivalent of NaN.
   *
   * MDN
   */
  val NaN: Double = native

  /**
   * The Number.NEGATIVE_INFINITY property represents the negative Infinity value.
   *
   * MDN
   */
  val NEGATIVE_INFINITY: Double = native
  /**
   * The Number.POSITIVE_INFINITY property represents the positive Infinity value.
   *
   * MDN
   */
  val POSITIVE_INFINITY: Double = native
}

/** Primitive JavaScript boolean.
 *
 *  In most situations, you should not need this trait, and use
 *  [[scala.Boolean]] instead.
 */
sealed trait Boolean extends Any {
  def unary_!(): scala.Boolean = native

  def &&(that: Boolean): Boolean = native
  def ||(that: Boolean): Boolean = native

  // See the comment in `Dynamic` for the rationale of returning Boolean here.
  def &&(that: Dynamic): Boolean = native
  def ||(that: Dynamic): Boolean = native
}

/** The top-level `Boolean` JavaScript object. */
object Boolean extends Object

/** Primitive JavaScript string.
 *
 *  In most situations, you should not need this trait, and use
 *  [[java.lang.String]] instead.
 */
sealed trait String extends Any {
  def +(that: String): String = native
  def +(that: Any): String = native
  def +(that: Dynamic): String = native

  def < (that: String): Boolean = native
  def < (that: Dynamic): Boolean = native

  def > (that: String): Boolean = native
  def > (that: Dynamic): Boolean = native

  def <=(that: String): Boolean = native
  def <=(that: Dynamic): Boolean = native

  def >=(that: String): Boolean = native
  def >=(that: Dynamic): Boolean = native

  /**
   * This property returns the number of code units in the string. UTF-16,
   * the string format used by JavaScript, uses a single 16-bit code unit to
   * represent the most common characters, but needs to use two code units for
   * less commonly-used characters, so it's possible for the value returned by
   * length to not match the actual number of characters in the string.
   *
   * For an empty string, length is 0.
   *
   * MDN
   */
  val length: Number = native

  /**
   * The chartAt() method returns the specified character from a string.
   *
   * Characters in a string are indexed from left to right. The index of the
   * first character is 0, and the index of the last character in a string
   * called stringName is stringName.length - 1. If the index you supply is out
   * of range, JavaScript returns an empty string.
   *
   * MDN
   */
  def charAt(pos: Number): String = native

  /**
   * The charCodeAt() method returns the numeric Unicode value of the character
   * at the given index (except for unicode codepoints > 0x10000).
   *
   * MDN
   */
  def charCodeAt(index: Number): Number = native

  /**
   * concat combines the text from one or more strings and returns a new string.
   * Changes to the text in one string do not affect the other string.
   * MDN
   */
  def concat(strings: String*): String = native

  /**
   * Returns the index within the calling String object of the first occurrence
   * of the specified value, starting the search at fromIndex,
   *
   * returns -1 if the value is not found.
   *
   * MDN
   */
  def indexOf(searchString: String, position: Number): Number = native
  def indexOf(searchString: String): Number = native

  /**
   * Returns the index within the calling String object of the last occurrence
   * of the specified value, or -1 if not found. The calling string is searched
   * backward, starting at fromIndex.
   *
   * MDN
   */
  def lastIndexOf(searchString: String, position: Number): Number = native
  def lastIndexOf(searchString: String): Number = native

  /**
   * Returns a number indicating whether a reference string comes before or
   * after or is the same as the given string in sort order. The new locales
   * and options arguments let applications specify the language whose sort
   * order should be used and customize the behavior of the function. In older
   * implementations, which ignore the locales and options arguments, the locale
   * and sort order used are entirely implementation dependent.
   *
   * MDN
   */
  def localeCompare(that: String): Number = native

  /**
   * Used to retrieve the matches when matching a string against a regular
   * expression.
   *
   * If the regular expression does not include the g flag, returns the same
   * result as regexp.exec(string). The returned Array has an extra input
   * property, which contains the original string that was parsed. In addition,
   * it has an index property, which represents the zero-based index of the
   * match in the string.
   *
   * If the regular expression includes the g flag, the method returns an Array
   * containing all matches. If there were no matches, the method returns null.
   *
   * MDN
   */
  def `match`(regexp: String): Array[String] = native
  def `match`(regexp: RegExp): Array[String] = native

  /**
   * Returns a new string with some or all matches of a pattern replaced by a
   * replacement.  The pattern can be a string or a RegExp, and the replacement
   * can be a string or a function to be called for each match.
   *
   * This method does not change the String object it is called on. It simply
   * returns a new string.
   *
   * To perform a global search and replace, either include the g switch in the
   * regular expression or if the first parameter is a string, include g in the
   * flags parameter.
   *
   * MDN
   */
  def replace(searchValue: String, replaceValue: String): String = native
  def replace(searchValue: String, replaceValue: Any): String = native
  def replace(searchValue: RegExp, replaceValue: String): String = native
  def replace(searchValue: RegExp, replaceValue: Any): String = native

  /**
   * If successful, search returns the index of the regular expression inside
   * the string. Otherwise, it returns -1.
   *
   * When you want to know whether a pattern is found in a string use search
   * (similar to the regular expression test method); for more information
   * (but slower execution) use match (similar to the regular expression exec
   * method).
   *
   * MDN
   */
  def search(regexp: String): Number = native
  def search(regexp: RegExp): Number = native

  /**
   * slice extracts the text from one string and returns a new string. Changes
   * to the text in one string do not affect the other string.
   *
   * slice extracts up to but not including endSlice. string.slice(1,4) extracts
   * the second character through the fourth character (characters indexed 1, 2,
   * and 3).
   *
   * As an example, string.slice(2,-1) extracts the third character through the
   * second to last character in the string.
   *
   * MDN
   */
  def slice(start: Number, end: Number): String = native
  def slice(start: Number): String = native

  /**
   * Splits a String object into an array of strings by separating the string
   * into substrings.
   *
   * When found, separator is removed from the string and the substrings are
   * returned in an array. If separator is omitted, the array contains one
   * element consisting of the entire string. If separator is an empty string,
   * string is converted to an array of characters.
   *
   * If separator is a regular expression that contains capturing parentheses,
   * then each time separator is matched, the results (including any undefined
   * results) of the capturing parentheses are spliced into the output array.
   * However, not all browsers support this capability.
   *
   * Note: When the string is empty, split returns an array containing one
   * empty string, rather than an empty array.
   *
   * MDN
   */
  def split(separator: String, limit: Number): Array[String] = native
  def split(separator: String): Array[String] = native
  def split(separator: RegExp, limit: Number): Array[String] = native
  def split(separator: RegExp): Array[String] = native

  /**
   * Returns a subset of a string between one index and another, or through
   * the end of the string.
   *
   * MDN
   */
  def substring(start: Number, end: Number): String = native
  def substring(start: Number): String = native

  /**
   * Returns the calling string value converted to lowercase.
   *
   * MDN
   */
  def toLowerCase(): String = native

  /**
   * The toLocaleLowerCase method returns the value of the string converted to
   * lower case according to any locale-specific case mappings. toLocaleLowerCase
   * does not affect the value of the string itself. In most cases, this will
   * produce the same result as toLowerCase(), but for some locales, such as
   * Turkish, whose case mappings do not follow the default case mappings in Unicode,
   * there may be a different result.
   *
   * MDN
   */
  def toLocaleLowerCase(): String = native

  /**
   * Returns the calling string value converted to uppercase.
   *
   * MDN
   */
  def toUpperCase(): String = native

  /**
   * The toLocaleUpperCase method returns the value of the string converted to
   * upper case according to any locale-specific case mappings. toLocaleUpperCase
   * does not affect the value of the string itself. In most cases, this will
   * produce the same result as toUpperCase(), but for some locales, such as
   * Turkish, whose case mappings do not follow the default case mappings in Unicode,
   * there may be a different result.
   *
   * MDN
   */
  def toLocaleUpperCase(): String = native

  /**
   * Removes whitespace from both ends of the string.
   *
   * MDN
   */
  def trim(): String = native
}

/** The top-level `String` JavaScript object. */
object String extends Object {
  def fromCharCode(codes: Int*): java.lang.String = native
}

/** Primitive JavaScript undefined value.
 *
 *  In most situations, you should not need this trait, and use
 *  [[scala.Unit]] instead.
 */
sealed trait Undefined extends Any

}
