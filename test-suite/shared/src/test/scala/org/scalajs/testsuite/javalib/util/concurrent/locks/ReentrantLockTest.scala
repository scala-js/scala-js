/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util.concurrent.locks

import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.TimeUnit
import java.lang.Thread

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

class ReentrantLockTest {

  @Test def should_lock_and_unlock(): Unit = {
    val lock = new ReentrantLock()
    assertFalse(lock.isLocked)
    lock.lock()
    assertTrue(lock.isLocked)
    lock.unlock()
    assertFalse(lock.isLocked)
  }

  @Test def properly_tryLock(): Unit = {
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

  @Test def properly_lockInterruptibly(): Unit = {
    val lock = new ReentrantLock()
    assertFalse(lock.isLocked)
    lock.lockInterruptibly()
    assertTrue(lock.isLocked)
    lock.unlock()
    assertFalse(lock.isLocked)
    Thread.currentThread().interrupt()
    assertThrows(classOf[InterruptedException], lock.lockInterruptibly)
  }

  @Test def check_if_is_held_by_current_Thread(): Unit = {
    val lock = new ReentrantLock()
    assertFalse(lock.isHeldByCurrentThread())
    lock.lock()
    assertTrue(lock.isHeldByCurrentThread())
  }

  @Test def should_be_created_with_a_fair_option(): Unit = {
    val l1 = new ReentrantLock()
    assertFalse(l1.isFair)
    val l2 = new ReentrantLock(false)
    assertFalse(l2.isFair)
    val l3 = new ReentrantLock(true)
    assertTrue(l3.isFair)
  }

  @Test def should_count_properly_number_of_locks(): Unit = {
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
