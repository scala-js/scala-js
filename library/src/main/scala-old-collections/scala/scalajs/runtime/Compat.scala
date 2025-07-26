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

  @inline def toGenericVarArgsWasmImpl[T](xs: Array[T]): Seq[T] =
    WrappedArray.make(xs)

  @inline def toRefVarArgsWasmImpl[T <: AnyRef](xs: Array[T]): Seq[T] =
    new WrappedArray.ofRef[T](xs)

  @inline def toUnitVarArgsWasmImpl(xs: Array[Unit]): Seq[Unit] =
    new WrappedArray.ofUnit(xs)

  @inline def toBooleanVarArgsWasmImpl(xs: Array[Boolean]): Seq[Boolean] =
    new WrappedArray.ofBoolean(xs)

  @inline def toCharVarArgsWasmImpl(xs: Array[Char]): Seq[Char] =
    new WrappedArray.ofChar(xs)

  @inline def toByteVarArgsWasmImpl(xs: Array[Byte]): Seq[Byte] =
    new WrappedArray.ofByte(xs)

  @inline def toShortVarArgsWasmImpl(xs: Array[Short]): Seq[Short] =
    new WrappedArray.ofShort(xs)

  @inline def toIntVarArgsWasmImpl(xs: Array[Int]): Seq[Int] =
    new WrappedArray.ofInt(xs)

  @inline def toLongVarArgsWasmImpl(xs: Array[Long]): Seq[Long] =
    new WrappedArray.ofLong(xs)

  @inline def toFloatVarArgsWasmImpl(xs: Array[Float]): Seq[Float] =
    new WrappedArray.ofFloat(xs)

  @inline def toDoubleVarArgsWasmImpl(xs: Array[Double]): Seq[Double] =
    new WrappedArray.ofDouble(xs)

}
