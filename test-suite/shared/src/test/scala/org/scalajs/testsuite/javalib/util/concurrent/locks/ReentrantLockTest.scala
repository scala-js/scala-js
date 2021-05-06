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

package org.scalajs.testsuite.javalib.util.concurrent.locks

import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.TimeUnit
import java.lang.Thread

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class ReentrantLockTest {

  @Test def lockAndUnlock(): Unit = {
    val lock = new ReentrantLock()
    assertFalse(lock.isLocked)
    lock.lock()
    assertTrue(lock.isLocked)
    lock.unlock()
    assertFalse(lock.isLocked)
  }

  @Test def tryLock(): Unit = {
    val lock = new ReentrantLock()
    assertFalse(lock.isLocked)
    lock.tryLock()
    assertTrue(lock.isLocked)
    lock.unlock()
    assertFalse(lock.isLocked)
    lock.tryLock(1L, TimeUnit.SECONDS)
    assertTrue(lock.isLocked)
    lock.unlock()
    assertFalse(lock.isLocked)
    Thread.currentThread().interrupt()
    assertThrows(classOf[InterruptedException], lock.tryLock(1L, TimeUnit.SECONDS))
  }

  @Test def lockInterruptibly(): Unit = {
    val lock = new ReentrantLock()
    assertFalse(lock.isLocked)
    lock.lockInterruptibly()
    assertTrue(lock.isLocked)
    lock.unlock()
    assertFalse(lock.isLocked)
    Thread.currentThread().interrupt()
    assertThrows(classOf[InterruptedException], lock.lockInterruptibly)
  }

  @Test def isHeldByCurrentThread(): Unit = {
    val lock = new ReentrantLock()
    assertFalse(lock.isHeldByCurrentThread())
    lock.lock()
    assertTrue(lock.isHeldByCurrentThread())
  }

  @Test def isFair(): Unit = {
    val l1 = new ReentrantLock()
    assertFalse(l1.isFair)
    val l2 = new ReentrantLock(false)
    assertFalse(l2.isFair)
    val l3 = new ReentrantLock(true)
    assertTrue(l3.isFair)
  }

  @Test def getHoldCount(): Unit = {
    val lock = new ReentrantLock()
    assertFalse(lock.isLocked)
    assertEquals(0, lock.getHoldCount())
    lock.lock()
    assertTrue(lock.isLocked)
    assertEquals(1, lock.getHoldCount())
    lock.lock()
    assertTrue(lock.isLocked)
    assertEquals(2, lock.getHoldCount())
    lock.unlock()
    assertTrue(lock.isLocked)
    assertEquals(1, lock.getHoldCount())
    lock.unlock()
    assertFalse(lock.isLocked)
    assertEquals(0, lock.getHoldCount())
    assertThrows(classOf[IllegalMonitorStateException], lock.unlock)
  }
}
