/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util.concurrent.locks

import scala.scalajs.js
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.TimeUnit
import java.lang.Thread

import org.scalajs.testsuite.utils.ExpectExceptions

object ReentrantLockTest extends ExpectExceptions {

  describe("java.util.concurrent.locks.ReentrantLock") {

    it("should lock and unlock") {
      val lock = new ReentrantLock()
      expect(lock.isLocked).toBeFalsy
      lock.lock()
      expect(lock.isLocked).toBeTruthy
      lock.unlock()
      expect(lock.isLocked).toBeFalsy
    }

    it("properly tryLock") {
      val lock = new ReentrantLock()
      expect(lock.isLocked).toBeFalsy
      lock.tryLock()
      expect(lock.isLocked).toBeTruthy
      lock.unlock()
      expect(lock.isLocked).toBeFalsy
      lock.tryLock(1L, TimeUnit.SECONDS)
      expect(lock.isLocked).toBeTruthy
      lock.unlock()
      expect(lock.isLocked).toBeFalsy
      Thread.currentThread().interrupt()
      expectThrows[InterruptedException](lock.tryLock(1L, TimeUnit.SECONDS))
    }

    it("properly lockInterruptibly") {
      val lock = new ReentrantLock()
      expect(lock.isLocked).toBeFalsy
      lock.lockInterruptibly()
      expect(lock.isLocked).toBeTruthy
      lock.unlock()
      expect(lock.isLocked).toBeFalsy
      Thread.currentThread().interrupt()
      expectThrows[InterruptedException](lock.lockInterruptibly)
    }

    it("check if is held by current Thread") {
      val lock = new ReentrantLock()
      expect(lock.isHeldByCurrentThread()).toBeFalsy
      lock.lock()
      expect(lock.isHeldByCurrentThread()).toBeTruthy
    }

    it("should be created with a fair option") {
      val l1 = new ReentrantLock()
      expect(l1.isFair).toBeFalsy
      val l2 = new ReentrantLock(false)
      expect(l2.isFair).toBeFalsy
      val l3 = new ReentrantLock(true)
      expect(l3.isFair).toBeTruthy
    }

    it("should count properly number of locks") {
      val lock = new ReentrantLock()
      expect(lock.isLocked).toBeFalsy
      expect(lock.getHoldCount()).toEqual(0)
      lock.lock()
      expect(lock.isLocked).toBeTruthy
      expect(lock.getHoldCount()).toEqual(1)
      lock.lock()
      expect(lock.isLocked).toBeTruthy
      expect(lock.getHoldCount()).toEqual(2)
      lock.unlock()
      expect(lock.isLocked).toBeTruthy
      expect(lock.getHoldCount()).toEqual(1)
      lock.unlock()
      expect(lock.isLocked).toBeFalsy
      expect(lock.getHoldCount()).toEqual(0)
      expectThrows[IllegalMonitorStateException](lock.unlock)
    }
  }
}
