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

package java.lang

import java.util.Iterator
import java.util.function.Consumer

trait Iterable[T] {
  def iterator(): Iterator[T]

  def forEach(action: Consumer[_ >: T]): Unit = {
    val iter = iterator()
    while (iter.hasNext())
      action.accept(iter.next())
  }
}
