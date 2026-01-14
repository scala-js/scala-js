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

package org.scalajs.linker

import scala.collection.mutable

private[linker] object CollectionsCompat {
  implicit class MutableMapCompatOps[K, V](private val self: mutable.Map[K, V]) extends AnyVal {

    // filterInPlace replaces retain
    def filterInPlace(p: (K, V) => Boolean): Unit = {
      // Believe it or not, this is the implementation of `retain` in 2.12.x:

      // scala/bug#7269 toList avoids ConcurrentModificationException
      for ((k, v) <- self.toList) {
        if (!p(k, v))
          self -= k
      }
    }
  }

  implicit class ArrayBufferCompatOps[V](private val self: mutable.ArrayBuffer[V]) extends AnyVal {

    def dropRightInPlace(n: Int): Unit =
      self.remove(self.length - n, n)
  }
}
