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

package java

package object util {

  private[util] final case class Box[+K](inner: K) {
    def apply(): K = inner

    override def equals(o: Any): Boolean = {
      o match {
        case o: Box[_] => Objects.equals(inner, o.inner)
        case _         => false
      }
    }

    override def hashCode(): Int =
      Objects.hashCode(inner)
  }

  private[util] def defaultOrdering[E]: Ordering[E] = {
    new Ordering[E] {
      def compare(a: E, b: E): Int =
        a.asInstanceOf[Comparable[E]].compareTo(b)
    }
  }
}
