/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.noircheck

import org.scalajs.jasminetest.JasmineTest

object DummyParentsTest extends JasmineTest {

  describe("Linking Stages") {

    it("should provide dummy parents if required") {

      import scala.concurrent.forkjoin._

      // scala.concurrent.forkjoin.ForkJoinWorkerThread is not defined
      class DummyFJWorkerThread extends ForkJoinWorkerThread(null) {
        override def onStart(): Unit = { /* something */ }
      }

      val x = "1".toInt

      if (x + x < 0) {
        // Ensure DummyFuture is not DCEd, but never instantiated
        new DummyFJWorkerThread()
      }
    }

  }
}
