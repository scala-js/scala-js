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

import java.util.concurrent.{Semaphore, TimeUnit}
import org.junit.Assert._
import org.junit.Test
import org.scalajs.testsuite.utils.AssertThrows._

class SemaphoreTest {

  @Test def ctorUnfair(): Unit = {
    val sem = new Semaphore(1)
    assertFalse(sem.isFair)
  }

  @Test def ctorNegativePermits(): Unit = {
    val sem = new Semaphore(-1)
    assertEquals(-1, sem.availablePermits)
    assertFalse(sem.tryAcquire())
    sem.release()
    assertEquals(0, sem.availablePermits)
  }

  @Test def drainNegative(): Unit = {
    val neg = -3
    val sem = new Semaphore(neg)
    assertEquals(neg, sem.drainPermits())
    assertEquals(0, sem.availablePermits)
  }

  @Test def tryAcquire(): Unit = {
    val sem = new Semaphore(1)
    assertTrue(sem.tryAcquire())
    assertEquals(0, sem.availablePermits)
  }

  @Test def tryAcquireNoneAvailable(): Unit = {
    val sem = new Semaphore(0)
    assertFalse(sem.tryAcquire())
  }

  @Test def tryAcquirePermitsNoneAvailable(): Unit = {
    val sem = new Semaphore(1)
    assertFalse(sem.tryAcquire(3))
  }

  @Test def tryAcquirePermits(): Unit = {
    val sem = new Semaphore(5)
    assertTrue(sem.tryAcquire(3))
    assertEquals(2, sem.availablePermits)
  }

  @Test def tryAcquireNegative(): Unit = {
    val sem = new Semaphore(0)
    assertThrows(classOf[IllegalArgumentException], sem.tryAcquire(-1))
  }

  @Test def release(): Unit = {
    val sem = new Semaphore(0)
    assertEquals(0, sem.availablePermits)
    sem.release()
    assertEquals(1, sem.availablePermits)
  }

  @Test def releasePermits(): Unit = {
    val sem = new Semaphore(1)
    assertEquals(1, sem.availablePermits)
    sem.release(2)
    assertEquals(3, sem.availablePermits)
  }

  @Test def releaseNegative(): Unit = {
    val sem = new Semaphore(0)
    assertThrows(classOf[IllegalArgumentException], sem.release(-1))
  }

}
