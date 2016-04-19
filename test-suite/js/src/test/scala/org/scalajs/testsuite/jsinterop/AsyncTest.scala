/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.language.implicitConversions

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.|

import scala.concurrent.{Future, ExecutionContext}
import scala.scalajs.concurrent._

import scala.collection.mutable.ArrayBuffer

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

class AsyncTest {

  implicit def eraseArray[T](a: Array[T]): Array[AnyRef] =
    a.map(_.asInstanceOf[AnyRef])

  def asyncTest(implicit executor: ExecutionContext): ArrayBuffer[String] = {
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
      implicit executor: ExecutionContext): Unit = {

    val res = asyncTest

    assertArrayEquals(Array(
      "prep-future",
      "prep-map",
      "prep-foreach",
      "done"), res.toArray)

    processQueue()

    assertArrayEquals(Array(
      "prep-future",
      "prep-map",
      "prep-foreach",
      "done",
      "future",
      "map",
      "foreach"), res.toArray)
  }

  @Test def scala_scalajs_concurrent_JSExecutionContext_queue(): Unit = {
    assumeTrue("Assumed js.Dynamic.global.Promise is undefined",
        js.isUndefined(js.Dynamic.global.Promise))
    TimeoutMock.withMockedTimeout { tick =>
      queueExecOrderTests { () =>
        tick(1)
      }(JSExecutionContext.queue)
    }
  }

  @Test def scala_scalajs_concurrent_JSExecutionContext_runNow(): Unit = {
    val res = asyncTest(JSExecutionContext.runNow)

    assertArrayEquals(Array(
      "prep-future",
      "future",
      "prep-map",
      "map",
      "prep-foreach",
      "foreach",
      "done"), res.toArray)
  }

  @Test def scala_scala_concurrent_ExecutionContext_global(): Unit = {
    assumeTrue("Assumed js.Dynamic.global.Promise is undefined",
        js.isUndefined(js.Dynamic.global.Promise))
    TimeoutMock.withMockedTimeout { tick =>
      queueExecOrderTests { () =>
        tick(1)
      }(ExecutionContext.global)

      assertSame(JSExecutionContext.queue, ExecutionContext.global)
    }
  }

  @Test def scala_scalajs_concurrent_QueueExecutionContext(): Unit = {
    TimeoutMock.withMockedTimeout { tick =>
      PromiseMock.withMockedPromiseIfExists { optProcessQueue =>
        implicit val executor = QueueExecutionContext()
        queueExecOrderTests { () =>
          tick(1)
          optProcessQueue.foreach(_())
        }
      }
    }
  }

  @Test def scala_scalajs_concurrent_QueueExecutionContext_timeouts(): Unit = {
    TimeoutMock.withMockedTimeout { tick =>
      implicit val executor = QueueExecutionContext.timeouts()
      queueExecOrderTests { () =>
        tick(1)
      }
    }
  }

  @Test def scala_scalajs_concurrent_QueueExecutionContext_promises(): Unit = {
    PromiseMock.withMockedPromise { processQueue =>
      implicit val executor = QueueExecutionContext.promises()
      queueExecOrderTests { () =>
        processQueue()
      }
    }
  }

  @Test def scala_concurrent_future_should_support_map(): Unit = {
      implicit val ec = JSExecutionContext.runNow
      val f = Future(3).map(x => x*2)
      assertEquals(6, f.value.get.get)
    }

  @Test def scala_concurrent_future_should_support_flatMap(): Unit = {
    implicit val ec = JSExecutionContext.runNow
    val f = Future(Future(3)).flatMap(x => x)
    assertEquals(3, f.value.get.get)
  }

  @Test def scala_concurrent_future_should_support_sequence(): Unit = {
    implicit val ec = JSExecutionContext.runNow
    val f = Future.sequence(Seq(Future(3), Future(5)))
    assertEquals(Seq(3, 5), f.value.get.get)
  }

  @Test def JSPromiseToFuture_basic_case(): Unit = {
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
  }

  @Test def scala_concurrent_FutureToJSPromise_basic_case(): Unit = {
    PromiseMock.withMockedPromise { processQueue =>
      implicit val ec = QueueExecutionContext.promises()

      val f = Future { 42 }
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
  }

  @Test def scala_concurrent_FutureToJSPromise_thenable_case(): Unit = {
    PromiseMock.withMockedPromise { processQueue =>
      implicit val ec = QueueExecutionContext.promises()

      val initialPromise = new js.Promise[Int]({
        (resolve: js.Function1[Int | js.Thenable[Int], _], reject: js.Function1[Any, _]) =>
          resolve(42)
      })

      val f = Future { initialPromise }
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
  }
}
