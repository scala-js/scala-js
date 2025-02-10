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

package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.|

import scala.scalajs.js.wasm.JSPI.allowOrphanJSAwait

import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global

import scala.collection.mutable.ArrayBuffer

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.junit.async._

class JSOrphanAwaitTest {
  @Test
  def basic(): AsyncResult = await {
    @noinline
    def foo(p: js.Promise[Int]): Int =
      js.await(p) * 3

    val p = js.Promise.resolve[Int](13).`then`(
        (i => i + 1): js.Function1[Int, Int | js.Thenable[Int]])

    val result = js.async {
      foo(p) - 1
    }

    result.toFuture.map { (result: Int) =>
      assertEquals(((13 + 1) * 3) - 1, result)
    }
  }

  @Test
  def throughClosure(): AsyncResult = await {
    @noinline
    def foo(p: js.Promise[Int])(op: js.Promise[Int] => Int): Int =
      op(p)

    val p = js.Promise.resolve[Int](13).`then`(
        (i => i + 1): js.Function1[Int, Int | js.Thenable[Int]])

    val result = js.async {
      foo(p)(p2 => js.await(p2) * 3) - 1
    }

    result.toFuture.map { (result: Int) =>
      assertEquals(((13 + 1) * 3) - 1, result)
    }
  }

  /* TODO Enable this test when browsers and Node.js implement the latest spec,
   * with SuspendError. At the time of writing, it passed in Chrome Canary.
   */
  @Test
  @org.junit.Ignore
  def suspendError(): AsyncResult = await {
    val p = js.Promise.resolve[Int](13)

    Future {
      try {
        js.await(p)
        throw new AssertionError(
            "js.await without an enclosing js.async should throw SuspendError")
      } catch {
        case js.JavaScriptException(e: js.Error) =>
          assertEquals("SuspendError", e.name)
          assertTrue(js.special.instanceof(e, js.Dynamic.global.WebAssembly.SuspendError))
      }
    }
  }
}
