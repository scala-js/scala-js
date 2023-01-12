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

import java.util.function._

// scalastyle:off equals.hash.code

/* A note about serializability:
 *
 * The JDK documentation states that returned comparators are serializable if
 * their respective elements (Comparators / Functions) are serializable.
 *
 * Experimentation on `nullsFirst` has shown that the returned comparator always
 * implements `Serializable` (and supposedly relies on the serialization
 * mechanism itself to fail when it is unable to serialize a field).
 *
 * Our implementation mimics this behavior.
 */

trait Comparator[A] { self =>
  import Comparator._

  def compare(o1: A, o2: A): Int
  def equals(obj: Any): Boolean

  def reversed(): Comparator[A] =
    Collections.reverseOrder(this)

  @inline
  def thenComparing(other: Comparator[_ >: A]): Comparator[A] = {
    other.getClass() // null check
    new Comparator[A] with Serializable {
      def compare(o1: A, o2: A) = {
        val cmp = self.compare(o1, o2)
        if (cmp != 0) cmp
        else other.compare(o1, o2)
      }
    }
  }

  def thenComparing[U](keyExtractor: Function[_ >: A, _ <: U],
      keyComparator: Comparator[_ >: U]): Comparator[A] = {
    thenComparing(comparing[A, U](keyExtractor, keyComparator))
  }

  /* Should be U <: Comparable[_ >: U] but scalac fails with
   * > illegal cyclic reference involving type U
   */
  def thenComparing[U <: Comparable[U]](
      keyExtractor: Function[_ >: A, _ <: U]): Comparator[A] = {
    thenComparing(comparing[A, U](keyExtractor))
  }

  def thenComparingInt(keyExtractor: ToIntFunction[_ >: A]): Comparator[A] =
    thenComparing(comparingInt(keyExtractor))

  def thenComparingLong(keyExtractor: ToLongFunction[_ >: A]): Comparator[A] =
    thenComparing(comparingLong(keyExtractor))

  def thenComparingDouble(keyExtractor: ToDoubleFunction[_ >: A]): Comparator[A] =
    thenComparing(comparingDouble(keyExtractor))

}

object Comparator {

  /* Should be T <: Comparable[_ >: T] but scalac fails with
   * > illegal cyclic reference involving type U
   */
  def reverseOrder[T <: Comparable[T]](): Comparator[T] =
    naturalOrder[T]().reversed()

  /* Should be T <: Comparable[_ >: T] but scalac fails with
   * > illegal cyclic reference involving type U
   */
  @inline
  def naturalOrder[T <: Comparable[T]](): Comparator[T] =
    NaturalComparator.asInstanceOf[Comparator[T]]

  @inline
  def nullsFirst[T](comparator: Comparator[_ >: T]): Comparator[T] = new Comparator[T] with Serializable {
    def compare(o1: T, o2: T): Int = {
      if (o1 == null && o2 == null) 0
      else if (o1 == null) -1
      else if (o2 == null) 1
      else if (comparator == null) 0
      else comparator.compare(o1, o2)
    }
  }

  @inline
  def nullsLast[T](comparator: Comparator[_ >: T]): Comparator[T] = new Comparator[T] with Serializable {
    def compare(o1: T, o2: T): Int = {
      if (o1 == null && o2 == null) 0
      else if (o1 == null) 1
      else if (o2 == null) -1
      else if (comparator == null) 0
      else comparator.compare(o1, o2)
    }
  }

  @inline
  def comparing[T, U](keyExtractor: Function[_ >: T, _ <: U],
      keyComparator: Comparator[_ >: U]): Comparator[T] = {
    keyExtractor.getClass() // null check
    keyComparator.getClass() // null check
    new Comparator[T] with Serializable {
      def compare(o1: T, o2: T): Int =
        keyComparator.compare(keyExtractor(o1), keyExtractor(o2))
    }
  }

  /* Should be U <: Comparable[_ >: U] but scalac fails with
   * > illegal cyclic reference involving type U
   */
  @inline
  def comparing[T, U <: Comparable[U]](
      keyExtractor: Function[_ >: T, _ <: U]): Comparator[T] = {
    keyExtractor.getClass() // null check
    new Comparator[T] with Serializable {
      def compare(o1: T, o2: T): Int =
        keyExtractor(o1).compareTo(keyExtractor(o2))
    }
  }

  @inline
  def comparingInt[T](keyExtractor: ToIntFunction[_ >: T]): Comparator[T] = {
    keyExtractor.getClass() // null check
    new Comparator[T] with Serializable {
      def compare(o1: T, o2: T): Int =
        Integer.compare(keyExtractor.applyAsInt(o1), keyExtractor.applyAsInt(o2))
    }
  }

  @inline
  def comparingLong[T](keyExtractor: ToLongFunction[_ >: T]): Comparator[T] = {
    keyExtractor.getClass() // null check
    new Comparator[T] with Serializable {
      def compare(o1: T, o2: T): Int =
        java.lang.Long.compare(keyExtractor.applyAsLong(o1), keyExtractor.applyAsLong(o2))
    }
  }

  @inline
  def comparingDouble[T](keyExtractor: ToDoubleFunction[_ >: T]): Comparator[T] = {
    keyExtractor.getClass() // null check
    new Comparator[T] with Serializable {
      def compare(o1: T, o2: T): Int =
        java.lang.Double.compare(keyExtractor.applyAsDouble(o1), keyExtractor.applyAsDouble(o2))
    }
  }
}
