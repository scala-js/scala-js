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

import scala.collection.immutable.ArraySeq
import scala.collection.IterableOnce

import scala.scalajs.js

private[runtime] object Compat {

  @inline def toScalaVarArgsImpl[A](array: js.Array[A]): Seq[A] =
    WrappedVarArgs.wrap(array)

  @inline def toScalaVarArgsFromScalaArrayAnyRefImpl(array: Array[AnyRef]): Seq[AnyRef] =
    new ArraySeq.ofRef(array)

  @inline def toScalaVarArgsFromScalaArrayIntImpl(array: Array[Int]): Seq[Int] =
    new ArraySeq.ofInt(array)

  @inline def toScalaVarArgsFromScalaArrayDoubleImpl(array: Array[Double]): Seq[Double] =
    new ArraySeq.ofDouble(array)

  @inline def toScalaVarArgsFromScalaArrayLongImpl(array: Array[Long]): Seq[Long] =
    new ArraySeq.ofLong(array)

  @inline def toScalaVarArgsFromScalaArrayFloatImpl(array: Array[Float]): Seq[Float] =
    new ArraySeq.ofFloat(array)

  @inline def toScalaVarArgsFromScalaArrayCharImpl(array: Array[Char]): Seq[Char] =
    new ArraySeq.ofChar(array)

  @inline def toScalaVarArgsFromScalaArrayByteImpl(array: Array[Byte]): Seq[Byte] =
    new ArraySeq.ofByte(array)

  @inline def toScalaVarArgsFromScalaArrayShortImpl(array: Array[Short]): Seq[Short] =
    new ArraySeq.ofShort(array)

  @inline def toScalaVarArgsFromScalaArrayBooleanImpl(array: Array[Boolean]): Seq[Boolean] =
    new ArraySeq.ofBoolean(array)

  @inline def toScalaVarArgsFromScalaArrayUnitImpl(array: Array[Unit]): Seq[Unit] =
    new ArraySeq.ofUnit(array)

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

}
