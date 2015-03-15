/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import scala.scalajs.js

import org.scalajs.jasminetest.JasmineTest

object ThreadTest extends JasmineTest {

  describe("java.lang.Thread") {
    it("getName and setName") {
      val t = Thread.currentThread()
      expect(t.getName).toBe("main") // default name of the main thread
      t.setName("foo")
      try {
        expect(t.getName).toBe("foo")
      } finally {
        t.setName("main") // don't pollute the rest of the world with this test
      }
      expect(t.getName).toBe("main")
    }

    it("Thread.currentThread().getStackTrace() should exist and not crash") {
      java.lang.Thread.currentThread().getStackTrace()
    }

    it("getId()") {
      expect(Thread.currentThread().getId > 0).toBeTruthy
    }
  }

}
