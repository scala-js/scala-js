/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.Platform.executingInJVM

class ThreadTest {

  @Test def getName_and_setName(): Unit = {
    if (!executingInJVM) {
      val t = Thread.currentThread()
      assertEquals("main", t.getName) // default name of the main thread
      t.setName("foo")
      try {
        assertEquals("foo", t.getName)
      } finally {
        t.setName("main") // don't pollute the rest of the world with this test
      }
      assertEquals("main", t.getName)
    }
  }

  @Test def currentThread_getStackTrace(): Unit = {
    Thread.currentThread().getStackTrace()
  }

  @Test def getId(): Unit = {
    assertTrue(Thread.currentThread().getId > 0)
  }

  @Test def interrupt_exist_and_the_status_is_properly_reflected(): Unit = {
    val t = Thread.currentThread()
    assertFalse(t.isInterrupted())
    assertFalse(Thread.interrupted())
    assertFalse(t.isInterrupted())
    t.interrupt()
    assertTrue(t.isInterrupted())
    assertTrue(Thread.interrupted())
    assertFalse(t.isInterrupted())
    assertFalse(Thread.interrupted())
  }
}
