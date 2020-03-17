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

package org.scalajs.testsuite.javalib.util.function

import java.{util => jul}
import java.util.concurrent._

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._

class ExecutorServiceTest {

  @Test def allMembers(): Unit = {
    assertTrue(ExecutorServiceTest.awaitTermination(0L, null.asInstanceOf[TimeUnit]))
    assertNull(ExecutorServiceTest.invokeAll(null.asInstanceOf[jul.Collection[Callable[Unit]]]))
    assertNull(ExecutorServiceTest.invokeAll(null.asInstanceOf[jul.Collection[Callable[Unit]]], 0L, null.asInstanceOf[TimeUnit]))
    assertNull(ExecutorServiceTest.invokeAny(null.asInstanceOf[jul.Collection[Callable[Unit]]]))
    assertNull(ExecutorServiceTest.invokeAny(null.asInstanceOf[jul.Collection[Callable[Unit]]], 0L, null.asInstanceOf[TimeUnit]))
    assertTrue(ExecutorServiceTest.isShutdown())
    assertTrue(ExecutorServiceTest.isTerminated())
    ExecutorServiceTest.shutdown()
    assertNull(ExecutorServiceTest.shutdownNow())
    assertNull(ExecutorServiceTest.submit(null.asInstanceOf[Callable[Unit]]))
    assertNull(ExecutorServiceTest.submit(null.asInstanceOf[Runnable]))
    assertNull(ExecutorServiceTest.submit(null.asInstanceOf[Runnable], ()))

    ExecutorServiceTest.execute(null.asInstanceOf[Runnable])
  }

}

object ExecutorServiceTest extends ExecutorService {
  def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = true
  def invokeAll[T](tasks: jul.Collection[_ <: Callable[T]]): jul.List[Future[T]] = null.asInstanceOf[jul.List[Future[T]]]
  def invokeAll[T](tasks: jul.Collection[_ <: Callable[T]], timeout: Long, unit: TimeUnit): jul.List[Future[T]] = null.asInstanceOf[jul.List[Future[T]]]
  def invokeAny[T](tasks: jul.Collection[_ <: Callable[T]]): T = null.asInstanceOf[T]
  def invokeAny[T](tasks: jul.Collection[_ <: Callable[T]], timeout: Long, unit: TimeUnit): T = null.asInstanceOf[T]
  def	isShutdown(): Boolean = true
  def	isTerminated(): Boolean = true
  def shutdown(): Unit = ()
  def shutdownNow(): jul.List[Runnable] = null.asInstanceOf[jul.List[Runnable]]
  def submit[T](task: Callable[T]): Future[T] = null.asInstanceOf[Future[T]]
  def submit(task: Runnable): Future[_] = null.asInstanceOf[Future[Unit]]
  def submit[T](task: Runnable, result: T): Future[T] = null.asInstanceOf[Future[T]]

  // inherited
  def execute(task: Runnable): Unit = ()
}
