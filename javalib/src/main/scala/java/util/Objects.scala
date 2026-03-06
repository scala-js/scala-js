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

package java.util

import java.util.function.Supplier

import scala.reflect.ClassTag

object Objects {

  @inline
  def equals(a: Any, b: Any): Boolean =
    if (a == null) b == null
    else a.equals(b)

  @inline
  def deepEquals(a: Any, b: Any): Boolean = {
    if (a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]) true
    else if (a == null || b == null) false
    else {
      (a, b) match {
        case (a1: Array[AnyRef], a2: Array[AnyRef])   => Arrays.deepEquals(a1, a2)
        case (a1: Array[Long], a2: Array[Long])       => Arrays.equals(a1, a2)
        case (a1: Array[Int], a2: Array[Int])         => Arrays.equals(a1, a2)
        case (a1: Array[Short], a2: Array[Short])     => Arrays.equals(a1, a2)
        case (a1: Array[Byte], a2: Array[Byte])       => Arrays.equals(a1, a2)
        case (a1: Array[Char], a2: Array[Char])       => Arrays.equals(a1, a2)
        case (a1: Array[Boolean], a2: Array[Boolean]) => Arrays.equals(a1, a2)
        case (a1: Array[Float], a2: Array[Float])     => Arrays.equals(a1, a2)
        case (a1: Array[Double], a2: Array[Double])   => Arrays.equals(a1, a2)
        case _                                        => Objects.equals(a, b)
      }
    }
  }

  @inline
  def hashCode(o: Any): Int =
    if (o == null) 0
    else o.hashCode()

  @inline
  def hash(values: Array[AnyRef]): Int =
    Arrays.hashCode(values)

  @inline
  def toString(o: Any): String =
    String.valueOf(o)

  @inline
  def toString(o: Any, nullDefault: String): String =
    if (o == null) nullDefault
    else o.toString

  @inline
  def compare[T](a: T, b: T, c: Comparator[_ >: T]): Int =
    if (a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]) 0
    else c.compare(a, b)

  @inline
  def requireNonNull[T](obj: T): T = {
    obj.getClass() // null check
    obj
  }

  @inline
  def requireNonNull[T](obj: T, message: String): T =
    if (obj == null) throwNPEWithMessage(message)
    else obj

  @inline
  def isNull(obj: Any): Boolean =
    obj == null

  @inline
  def nonNull(obj: Any): Boolean =
    obj != null

  @inline
  def requireNonNull[T](obj: T, messageSupplier: Supplier[String]): T =
    if (obj == null) throwNPEWithMessage(messageSupplier.get())
    else obj

  @noinline private def throwNPEWithMessage(message: String): Nothing = {
    /* This method is our best attempt to deal with the overloads of
     * `requireNonNull` with an explicit message. We want to trigger a UB NPE.
     * However, if we do that, we lose the `message`. This is fine for the
     * Unchecked behavior, debatable for Fatal, and plain wrong for Compliant.
     *
     * To recover the message in Compliant mode, we immediately catch a genuine
     * NPE if that is what the UB throws, and rethrow a genuine NPE with the
     * correct message.
     *
     * In Fatal mode, there is unfortunately nothing we can do. We have no way
     * of constructing another UndefinedBehaviorError with a different cause.
     */
    try {
      throw null
    } catch {
      case _: NullPointerException =>
        throw new NullPointerException(message)
    }
  }
}
