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

import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global

import scala.collection.mutable.ArrayBuffer

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.junit.async._

class JSAsyncAwaitTest {
  @Test
  def basic(): AsyncResult = await {
    val buf = new ArrayBuffer[String]()

    val px = js.Promise.resolve[Int](5)

    buf += "before"
    val p = js.async {
      buf += "start async"
      val x = js.await(px)
      buf += s"got x: $x"
      x + 13
    }
    buf += "after"

    assertArrayEquals(
        Array[AnyRef]("before", "start async", "after"),
        buf.toArray[AnyRef])

    p.toFuture.map { (result: Int) =>
      assertEquals(18, result)

      assertArrayEquals(
          Array[AnyRef]("before", "start async", "after", "got x: 5"),
          buf.toArray[AnyRef])
    }
  }

  @Test
  def loop(): AsyncResult = await {
    val buf = new ArrayBuffer[String]()

    val inputs = (1 to 5).toList
    val inputPromises = inputs.map(i => js.Promise.resolve[Int](i))

    buf += "before"
    val p = js.async {
      buf += "start async"
      var rest = inputPromises
      while (!rest.isEmpty) {
        val x = js.await(rest.head)
        buf += x.toString()
        rest = rest.tail
      }
      buf += "done"
    }
    buf += "after"

    assertArrayEquals(
        Array[AnyRef]("before", "start async", "after"),
        buf.toArray[AnyRef])

    p.toFuture.map { _ =>
      assertArrayEquals(
          ("before" :: "start async" :: "after" :: inputs.map(
              _.toString()) ::: "done" :: Nil).toArray[AnyRef],
          buf.toArray[AnyRef])
    }
  }

  @Test
  def tryCatch(): AsyncResult = await {
    val successfulInput = js.Promise.resolve[Int](42)
    val failedInput: js.Promise[Int] = js.Promise.reject(new IllegalArgumentException("nope"))

    val p = js.async {
      val result1 = try {
        js.await(successfulInput)
      } catch {
        case e: IllegalArgumentException =>
          throw new AssertionError(e)
      }
      val result2 = try {
        js.await(failedInput)
        throw new AssertionError("awaiting a failed Promise did not throw")
      } catch {
        case e: IllegalArgumentException =>
          56
      }
      (result1, result2)
    }

    p.toFuture.map { case (result1, result2) =>
      assertEquals(42, result1)
      assertEquals(56, result2)
    }
  }

  @Test
  def tryFinally(): AsyncResult = await {
    val successfulInput = js.Promise.resolve[Int](42)
    val failedInput: js.Promise[Int] = js.Promise.reject(new IllegalArgumentException("nope"))

    val buf = new ArrayBuffer[String]()

    val p: js.Promise[Int] = js.async {
      val result1 = try {
        js.await(successfulInput)
      } finally {
        buf += "first"
      }
      assertEquals(42, result1)
      val result2 = try {
        js.await(failedInput)
        throw new AssertionError("awaiting a failed Promise did not throw")
      } finally {
        buf += "second"
      }
      throw new AssertionError("did not rethrow after the finally")
    }

    p.toFuture.failed.map { e =>
      assertTrue(e.toString(), e.isInstanceOf[IllegalArgumentException])
      assertArrayEquals(
          Array[AnyRef]("first", "second"),
          buf.toArray[AnyRef])
    }
  }
}
