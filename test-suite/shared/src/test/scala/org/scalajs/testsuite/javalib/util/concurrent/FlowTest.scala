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

package org.scalajs.testsuite.javalib.util.concurrent

import java.util.concurrent.Flow

import org.junit.Test
import org.junit.Assert._

class FlowTest {
  import FlowTest._

  @Test def testDefaultBufferSize(): Unit =
    assertEquals(256, Flow.defaultBufferSize())

  @Test def testProcessor(): Unit = {
    val processor = makeProcessor[Int, String]()
    processor.subscribe(makeSubscriber[String]())
    processor.onSubscribe(makeSubscription())
    processor.onNext(42)
    processor.onError(new Exception)
    processor.onComplete()
  }

  @Test def testPublisher(): Unit = {
    val publisher = makePublisher[Int]()
    publisher.subscribe(makeSubscriber[Int]())
  }

  @Test def testSubscriber(): Unit = {
    val subscriber = makeSubscriber[Int]()
    subscriber.onSubscribe(makeSubscription())
    subscriber.onNext(42)
    subscriber.onError(new Exception)
    subscriber.onComplete()
  }

  @Test def testSubscription(): Unit = {
    val subscription = makeSubscription()
    subscription.request(42)
    subscription.cancel()
  }

}

object FlowTest {

  def makeProcessor[T, R](): Flow.Processor[T, R] = {
    new Flow.Processor[T, R] {
      def subscribe(subscriber: Flow.Subscriber[_ >: R]): Unit = ()
      def onSubscribe(subscription: Flow.Subscription): Unit = ()
      def onNext(item: T): Unit = ()
      def onError(throwable: Throwable): Unit = ()
      def onComplete(): Unit = ()
    }
  }

  def makePublisher[T](): Flow.Publisher[T] = {
    new Flow.Publisher[T] {
      def subscribe(subscriber: Flow.Subscriber[_ >: T]): Unit = ()
    }
  }

  def makeSubscriber[T](): Flow.Subscriber[T] = {
    new Flow.Subscriber[T] {
      def onSubscribe(subscription: Flow.Subscription): Unit = ()
      def onNext(item: T): Unit = ()
      def onError(throwable: Throwable): Unit = ()
      def onComplete(): Unit = ()
    }
  }

  def makeSubscription(): Flow.Subscription = {
    new Flow.Subscription {
      def request(n: Long): Unit = ()
      def cancel(): Unit = ()
    }
  }

}
