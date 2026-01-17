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
      |If you compile for WebAssembly, you can allow arbitrary js.await()
      |calls by adding the following import:
      |import scala.scalajs.js.wasm.JSPI.allowOrphanJSAwait
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
      |If you compile for WebAssembly, you can allow arbitrary js.await()
      |calls by adding the following import:
      |import scala.scalajs.js.wasm.JSPI.allowOrphanJSAwait
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
      |If you compile for WebAssembly, you can allow arbitrary js.await()
      |calls by adding the following import:
      |import scala.scalajs.js.wasm.JSPI.allowOrphanJSAwait
      |        def f(): Int = js.await(x)
      |                               ^
    """
  }
}
