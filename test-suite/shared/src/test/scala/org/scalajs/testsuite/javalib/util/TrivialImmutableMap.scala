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

package org.scalajs.testsuite.javalib.util

import java.{util => ju}
import java.util.Map.Entry

final class TrivialImmutableMap[K, V] private (contents: List[Entry[K, V]])
    extends ju.AbstractMap[K, V] {

  def entrySet(): ju.Set[Entry[K,V]] = {
    new ju.AbstractSet[Entry[K, V]] {
      def size(): Int = contents.size

      def iterator(): ju.Iterator[Entry[K,V]] = {
        new ju.Iterator[Entry[K, V]] {
          private var remaining: List[Entry[K, V]] = contents

          def hasNext(): Boolean = remaining.nonEmpty

          def next(): Entry[K,V] = {
            val head = remaining.head
            remaining = remaining.tail
            head
          }
        }
      }
    }
  }
}

object TrivialImmutableMap {
  def apply[K, V](contents: List[Entry[K, V]]): TrivialImmutableMap[K, V] =
    new TrivialImmutableMap(contents)

  def apply[K, V](contents: (K, V)*): TrivialImmutableMap[K, V] =
    apply(contents.toList.map(kv => new ju.AbstractMap.SimpleImmutableEntry(kv._1, kv._2)))
}
