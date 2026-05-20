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
import scala.scalajs.LinkingInfo.{linkTimeIf, moduleKind}
import scala.scalajs.LinkingInfo.ModuleKind.MinimalWasmModule

import scala.concurrent.{Future, ExecutionContext}
import scala.scalajs.concurrent._

import scala.collection.mutable.ArrayBuffer

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.junit.async._

class AsyncTest {
  def asyncTest(implicit ec: ExecutionContext): ArrayBuffer[String] = {
    val steps = new ArrayBuffer[String]

    steps += "prep-future"

    val f1 = Future {
      steps += "future"
      1 + 2 + 3
    }

    steps += "prep-map"

    val f2 = f1 map { x =>
      steps += "map"
      x * 2
    }

    steps += "prep-foreach"

    f2 foreach { _ => steps += "foreach" }

    steps += "done"

    steps
  }

  def queueExecOrderTests(processQueue: () => Unit)(
      implicit ec: ExecutionContext): Unit = {

    val res = asyncTest

    assertArrayEquals(
        Array[AnyRef](
            "prep-future",
            "prep-map",
            "prep-foreach",
            "done"),
        res.toArray[AnyRef])

    processQueue()

    assertQueueExecOrderCompleted(res)
  }

  private def assertQueueExecOrderCompleted(res: ArrayBuffer[String]): Unit = {
    assertArrayEquals(
        Array[AnyRef](
            "prep-future",
            "prep-map",
            "prep-foreach",
            "done",
            "future",
            "map",
            "foreach"),
        res.toArray[AnyRef])
  }

  // format: off
  // scalastyle:off line.size.limit

  @Test def scalaScalajsConcurrentJSExecutionContextQueue(): Unit = { linkTimeIf(moduleKind != MinimalWasmModule) {
    assumeTrue("Assumed js.Dynamic.global.Promise is undefined",
        js.typeOf(js.Dynamic.global.Promise) == "undefined")
    TimeoutMock.withMockedTimeout { tick =>
      queueExecOrderTests { () =>
        tick(1)
      }(JSExecutionContext.queue)
    }
  }{()}}

  @Test def scalaScalaConcurrentExecutionContextGlobal(): Unit = { linkTimeIf(moduleKind != MinimalWasmModule) {
    assumeTrue("Assumed js.Dynamic.global.Promise is undefined",
        js.typeOf(js.Dynamic.global.Promise) == "undefined")
    TimeoutMock.withMockedTimeout { tick =>
      queueExecOrderTests { () =>
        tick(1)
      }(ExecutionContext.global)

      assertSame(JSExecutionContext.queue, ExecutionContext.global)
    }
  }{()}}

  @Test def scalaScalajsConcurrentQueueExecutionContext(): Unit = { linkTimeIf(moduleKind != MinimalWasmModule) {
    TimeoutMock.withMockedTimeout { tick =>
      PromiseMock.withMockedPromiseIfExists { optProcessQueue =>
        implicit val executor = QueueExecutionContext()
        queueExecOrderTests { () =>
          tick(1)
          optProcessQueue.foreach(_())
        }
      }
    }
  }{()}}

  @Test def scalaScalajsConcurrentQueueExecutionContextSingleThreaded(): Unit = {
    implicit val ec: ExecutionContext = QueueExecutionContext.single()
    Future {
      asyncTest
    }.foreach(assertQueueExecOrderCompleted)
  }

  @Test def scalaScalajsConcurrentQueueExecutionContextTimeouts(): Unit = { linkTimeIf(moduleKind != MinimalWasmModule) {
    TimeoutMock.withMockedTimeout { tick =>
      implicit val executor = QueueExecutionContext.timeouts()
      queueExecOrderTests { () =>
        tick(1)
      }
    }
  }{()}}

  @Test def scalaScalajsConcurrentQueueExecutionContextPromises(): Unit = { linkTimeIf(moduleKind != MinimalWasmModule) {
    PromiseMock.withMockedPromise { processQueue =>
      implicit val executor = QueueExecutionContext.promises()
      queueExecOrderTests { () =>
        processQueue()
      }
    }
  }{()}}

  @Test def scalaConcurrentFutureSupportsMap(): AsyncResult = await {
    import ExecutionContext.Implicits.global
    val f = Future(3).map(x => x * 2)
    f.map(v => assertEquals(6, v))
  }

  @Test def scalaConcurrentFutureSupportsFlatMap(): AsyncResult = await {
    import ExecutionContext.Implicits.global
    val f = Future(Future(3)).flatMap(x => x)
    f.map(v => assertEquals(3, v))
  }

  @Test def scalaConcurrentFutureSupportsSequence(): AsyncResult = await {
    import ExecutionContext.Implicits.global
    val f = Future.sequence(Seq(Future(3), Future(5)))
    f.map(v => assertEquals(Seq(3, 5), v))
  }

  @Test def jsPromiseToFutureBasicCase(): Unit = { linkTimeIf(moduleKind != MinimalWasmModule) {
    PromiseMock.withMockedPromise { processQueue =>
      implicit val ec = QueueExecutionContext.promises()

      val p = new js.Promise[Int]({
        (resolve: js.Function1[Int | js.Thenable[Int], _], reject: js.Function1[Any, _]) =>
          resolve(42)
      })

      val f = p.toFuture
      val fAssertType: Future[Int] = f

      var callbackDone = false

      fAssertType.foreach { x =>
        assertEquals(42, x)
        callbackDone = true
      }

      processQueue()

      assertTrue(callbackDone)
    }
  }{()}}

  @Test def scalaConcurrentFutureToJSPromiseBasicCase(): Unit = { linkTimeIf(moduleKind != MinimalWasmModule) {
    PromiseMock.withMockedPromise { processQueue =>
      implicit val ec = QueueExecutionContext.promises()

      val f = Future(42)
      val p = f.toJSPromise
      val pAssertType: js.Promise[Int] = p

      var callbackDone = false

      pAssertType.`then`[Unit] { (x: Int) =>
        assertEquals(42, x)
        callbackDone = true
        (): Unit | js.Thenable[Unit]
      }

      processQueue()

      assertTrue(callbackDone)
    }
  }{()}}

  @Test def scalaConcurrentFutureToJSPromiseThenableCase(): Unit = { linkTimeIf(moduleKind != MinimalWasmModule) {
    PromiseMock.withMockedPromise { processQueue =>
      implicit val ec = QueueExecutionContext.promises()

      val initialPromise = new js.Promise[Int]({
        (resolve: js.Function1[Int | js.Thenable[Int], _], reject: js.Function1[Any, _]) =>
          resolve(42)
      })

      val f = Future(initialPromise)
      val p = f.toJSPromise
      val pAssertType: js.Promise[Int] = p

      var callbackDone = false

      pAssertType.`then`[Unit] { (x: Int) =>
        assertEquals(42, x)
        callbackDone = true
        (): Unit | js.Thenable[Unit]
      }

      processQueue()

      assertTrue(callbackDone)
    }
  }{()}}

  // scalastyle:on line.size.limit
  // format: on
}
