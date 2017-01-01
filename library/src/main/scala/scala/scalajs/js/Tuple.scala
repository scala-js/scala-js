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

import scala.scalajs.js.annotation.JSName
import scala.language.implicitConversions

/**
 *  A tuple "view" of 2 elements of a JavaScript [[Array]].
 *  Combines
 *  {{{
 *  0: T0;
 *  1: T1;
 *  }}}
 *  to
 *  {{{
 *  js.Tuple2[T0,T1]
 *  }}}
 *
 *  Supports implicit conversion to [[scala.Tuple2]].
 *  To use it, cast your array into a [[Tuple2]] using
 *  {{{
 *  val array = js.Array[Any](42, "foobar")
 *  val tuple2 = array.asInstanceOf[js.Tuple2[Int, String]]
 *  }}}
 *  or convert a Scala tuple
 *  {{{
 *  val obj: js.Tuple2[Int, String] = (42, "foobar")
 *  }}}
 */
@native
sealed trait Tuple2[+T1, +T2] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
}

object Tuple2 {
  @inline def apply[T1, T2](_1: T1, _2: T2): Tuple2[T1, T2] =
    Array(_1, _2).asInstanceOf[Tuple2[T1, T2]]

  @inline def unapply[T1, T2](t: Tuple2[T1, T2]): Option[(T1, T2)] =
    Some(t)

  @inline implicit def fromScalaTuple2[T1, T2](t: (T1, T2)): Tuple2[T1, T2] =
    apply(t._1, t._2)

  @inline implicit def toScalaTuple2[T1, T2](t: Tuple2[T1, T2]): (T1, T2) =
    (t._1, t._2)
}

/**
 *  A tuple "view" of 3 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple3[+T1, +T2, +T3] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
}

object Tuple3 {
  @inline def apply[T1, T2, T3](_1: T1, _2: T2, _3: T3): Tuple3[T1, T2, T3] =
    Array(_1, _2, _3).asInstanceOf[Tuple3[T1, T2, T3]]

  @inline def unapply[T1, T2, T3](t: Tuple3[T1, T2, T3]): Option[(T1, T2, T3)] =
    Some(t)

  @inline implicit def fromScalaTuple3[T1, T2, T3](t: (T1, T2, T3)): Tuple3[T1, T2, T3] =
    apply(t._1, t._2, t._3)

  @inline implicit def toScalaTuple3[T1, T2, T3](t: Tuple3[T1, T2, T3]): (T1, T2, T3) =
    (t._1, t._2, t._3)
}
