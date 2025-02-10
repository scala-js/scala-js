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

package org.scalajs.nscplugin.test

import org.scalajs.nscplugin.test.util._
import org.junit.Test

// scalastyle:off line.size.limit

class JSAsyncAwaitTest extends DirectTest with TestHelpers {

  override def preamble: String =
    """import scala.scalajs.js
    """

  @Test
  def orphanAwait(): Unit = {
    """
    class A {
      def foo(x: js.Promise[Int]): Int =
        js.await(x)
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: Illegal use of js.await().
      |It can only be used inside a js.async {...} block, without any lambda,
      |by-name argument or nested method in-between.
      |        js.await(x)
      |                ^
    """

    """
    class A {
      def foo(x: js.Promise[Int]): js.Promise[Int] = js.async {
        val f: () => Int = () => js.await(x)
        f()
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: Illegal use of js.await().
      |It can only be used inside a js.async {...} block, without any lambda,
      |by-name argument or nested method in-between.
      |        val f: () => Int = () => js.await(x)
      |                                         ^
    """

    """
    class A {
      def foo(x: js.Promise[Int]): js.Promise[Int] = js.async {
        def f(): Int = js.await(x)
        f()
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: Illegal use of js.await().
      |It can only be used inside a js.async {...} block, without any lambda,
      |by-name argument or nested method in-between.
      |        def f(): Int = js.await(x)
      |                               ^
    """
  }
}
