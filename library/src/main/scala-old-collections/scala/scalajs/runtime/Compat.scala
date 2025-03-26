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

package scala.scalajs.runtime

import scala.collection.GenTraversableOnce
import scala.collection.mutable.WrappedArray
import scala.scalajs.js

private[runtime] object Compat {

  @inline def toScalaVarArgsImpl[A](array: js.Array[A]): Seq[A] =
    new js.WrappedArray(array)

  @inline def toScalaVarArgsFromScalaArrayAnyRefImpl(array: Array[AnyRef]): Seq[AnyRef] =
    new WrappedArray.ofRef(array)

  @inline def toScalaVarArgsFromScalaArrayIntImpl(array: Array[Int]): Seq[Int] =
    new WrappedArray.ofInt(array)

  @inline def toScalaVarArgsFromScalaArrayDoubleImpl(array: Array[Double]): Seq[Double] =
    new WrappedArray.ofDouble(array)

  @inline def toScalaVarArgsFromScalaArrayLongImpl(array: Array[Long]): Seq[Long] =
    new WrappedArray.ofLong(array)

  @inline def toScalaVarArgsFromScalaArrayFloatImpl(array: Array[Float]): Seq[Float] =
    new WrappedArray.ofFloat(array)

  @inline def toScalaVarArgsFromScalaArrayCharImpl(array: Array[Char]): Seq[Char] =
    new WrappedArray.ofChar(array)

  @inline def toScalaVarArgsFromScalaArrayByteImpl(array: Array[Byte]): Seq[Byte] =
    new WrappedArray.ofByte(array)

  @inline def toScalaVarArgsFromScalaArrayShortImpl(array: Array[Short]): Seq[Short] =
    new WrappedArray.ofShort(array)

  @inline def toScalaVarArgsFromScalaArrayBooleanImpl(array: Array[Boolean]): Seq[Boolean] =
    new WrappedArray.ofBoolean(array)

  @inline def toScalaVarArgsFromScalaArrayUnitImpl(array: Array[Unit]): Seq[Unit] =
    new WrappedArray.ofUnit(array)


  def toJSVarArgsImpl[A](seq: Seq[A]): js.Array[A] = {
    seq match {
      case seq: js.WrappedArray[A] =>
        js.WrappedArray.toJSArray(seq)

      case _ =>
        val result = new js.Array[A]
        seq.foreach(x => result.push(x))
        result
    }
  }

}
