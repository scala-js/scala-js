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

package java.util.concurrent

object Flow {

  @inline def defaultBufferSize(): Int = 256

  trait Processor[T, R] extends Subscriber[T] with Publisher[R]

  @FunctionalInterface
  trait Publisher[T] {
    def subscribe(subscriber: Subscriber[_ >: T]): Unit
  }

  trait Subscriber[T] {
    def onSubscribe(subscription: Subscription): Unit
    def onNext(item: T): Unit
    def onError(throwable: Throwable): Unit
    def onComplete(): Unit
  }

  trait Subscription {
    def request(n: Long): Unit
    def cancel(): Unit
  }

}
