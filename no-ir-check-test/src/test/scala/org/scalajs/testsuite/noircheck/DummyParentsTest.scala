/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.noircheck

import org.junit.Test

class DummyParentsTest {

  @Test def linking_stages_should_provide_dummy_parents_if_required(): Unit = {

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
