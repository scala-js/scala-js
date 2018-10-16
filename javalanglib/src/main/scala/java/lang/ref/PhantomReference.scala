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

package java.lang.ref

class PhantomReference[T >: Null <: AnyRef](referent: T,
    queue: ReferenceQueue[_ >: T]) extends Reference[T](null) {

  override def get(): T = null
}
