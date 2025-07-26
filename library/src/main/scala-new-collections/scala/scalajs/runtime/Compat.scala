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

import scala.collection.IterableOnce
import scala.collection.immutable.ArraySeq

import scala.scalajs.js

private[runtime] object Compat {

  @inline def toScalaVarArgsImpl[A](array: js.Array[A]): Seq[A] =
    WrappedVarArgs.wrap(array)

  def toJSVarArgsImpl[A](seq: Seq[A]): js.Array[A] = {
    seq match {
      case seq: WrappedVarArgs[A] =>
        seq.unwrap.asInstanceOf[js.Array[A]]
      case _ =>
        val result = new js.Array[A]
        seq.foreach(x => result.push(x))
        result
    }
  }

  @inline def toGenericVarArgsWasmImpl[T](xs: Array[T]): Seq[T] =
    ArraySeq.unsafeWrapArray(xs)

  @inline def toRefVarArgsWasmImpl[T <: AnyRef](xs: Array[T]): Seq[T] =
    new ArraySeq.ofRef[T](xs)

  @inline def toUnitVarArgsWasmImpl(xs: Array[Unit]): Seq[Unit] =
    new ArraySeq.ofUnit(xs)

  @inline def toBooleanVarArgsWasmImpl(xs: Array[Boolean]): Seq[Boolean] =
    new ArraySeq.ofBoolean(xs)

  @inline def toCharVarArgsWasmImpl(xs: Array[Char]): Seq[Char] =
    new ArraySeq.ofChar(xs)

  @inline def toByteVarArgsWasmImpl(xs: Array[Byte]): Seq[Byte] =
    new ArraySeq.ofByte(xs)

  @inline def toShortVarArgsWasmImpl(xs: Array[Short]): Seq[Short] =
    new ArraySeq.ofShort(xs)

  @inline def toIntVarArgsWasmImpl(xs: Array[Int]): Seq[Int] =
    new ArraySeq.ofInt(xs)

  @inline def toLongVarArgsWasmImpl(xs: Array[Long]): Seq[Long] =
    new ArraySeq.ofLong(xs)

  @inline def toFloatVarArgsWasmImpl(xs: Array[Float]): Seq[Float] =
    new ArraySeq.ofFloat(xs)

  @inline def toDoubleVarArgsWasmImpl(xs: Array[Double]): Seq[Double] =
    new ArraySeq.ofDouble(xs)

}
