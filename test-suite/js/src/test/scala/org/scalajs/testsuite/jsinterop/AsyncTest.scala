/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.|

import org.scalajs.jasminetest.JasmineTest

import scala.concurrent.{Future, ExecutionContext}
import scala.scalajs.concurrent._

import scala.collection.mutable.ArrayBuffer

import org.scalajs.jasmine.JasmineExpectation

object AsyncTest extends JasmineTest {

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

  def expect(abuf: ArrayBuffer[String]): JasmineExpectation =
    expect(abuf.toJSArray)

  def establishedQueueExecOrderTests()(
      implicit executor: ExecutionContext): Unit = {

    if (js.isUndefined(js.Dynamic.global.Promise)) {
      it("should correctly order future calls") {
        queueExecOrderTests { () =>
          jasmine.Clock.tick(1)
        }
      }
    } else {
      /* Ignore, because we cannot mock Promise after the
       * established QueueExecutionContext has been created.
       */
    }
  }

  def queueExecOrderTests(processQueue: () => Unit)(
      implicit executor: ExecutionContext): Unit = {

    val res = asyncTest

    expect(res).toEqual(js.Array(
      "prep-future",
      "prep-map",
      "prep-foreach",
      "done"))

    processQueue()

    expect(res).toEqual(js.Array(
      "prep-future",
      "prep-map",
      "prep-foreach",
      "done",
      "future",
      "map",
      "foreach"))
  }

  describe("scala.scalajs.concurrent.JSExecutionContext.queue") {

    beforeEach {
      jasmine.Clock.useMock()
    }

    establishedQueueExecOrderTests()(JSExecutionContext.queue)

  }

  describe("scala.scalajs.concurrent.JSExecutionContext.runNow") {

    it("should correctly order future calls") {
      val res = asyncTest(JSExecutionContext.runNow)

      expect(res).toEqual(js.Array(
          "prep-future",
          "future",
          "prep-map",
          "map",
          "prep-foreach",
          "foreach",
          "done"))
    }

  }

  describe("scala.scala.concurrent.ExecutionContext.global") {

    beforeEach {
      jasmine.Clock.useMock()
    }

    it("should be a queue execution context") {
      expect(ExecutionContext.global eq JSExecutionContext.queue).toBeTruthy
    }

    establishedQueueExecOrderTests()(ExecutionContext.global)

  }

  describe("scala.scalajs.concurrent.QueueExecutionContext()") {

    beforeEach {
      jasmine.Clock.useMock()
    }

    it("should correctly order future calls") {
      PromiseMock.withMockedPromiseIfExists { optProcessQueue =>
        implicit val executor = QueueExecutionContext()
        queueExecOrderTests { () =>
          jasmine.Clock.tick(1)
          optProcessQueue.foreach(_())
        }
      }
    }

  }

  describe("scala.scalajs.concurrent.QueueExecutionContext.timeouts()") {

    beforeEach {
      jasmine.Clock.useMock()
    }

    it("should correctly order future calls") {
      implicit val executor = QueueExecutionContext.timeouts()
      queueExecOrderTests { () =>
        jasmine.Clock.tick(1)
      }
    }

  }

  describe("scala.scalajs.concurrent.QueueExecutionContext.promises()") {

    it("should correctly order future calls") {
      PromiseMock.withMockedPromise { processQueue =>
        implicit val executor = QueueExecutionContext.promises()
        queueExecOrderTests { () =>
          processQueue()
        }
      }
    }

  }

  describe("scala.concurrent.Future") {

    it("should support map") {
      implicit val ec = JSExecutionContext.runNow
      val f = Future(3).map(x => x*2)
      expect(f.value.get.get).toEqual(6)
    }

    it("should support flatMap") {
      implicit val ec = JSExecutionContext.runNow
      val f = Future(Future(3)).flatMap(x => x)
      expect(f.value.get.get).toEqual(3)
    }

    it("should support sequence") {
      implicit val ec = JSExecutionContext.runNow
      val f = Future.sequence(Seq(Future(3), Future(5)))
      expect(f.value.get.get.toJSArray).toEqual(js.Array(3, 5))
    }

  }

  describe("js.Promise.toFuture") {
    it("basic case") {
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
          expect(x).toEqual(42)
          callbackDone = true
        }

        processQueue()

        expect(callbackDone).toBeTruthy
      }
    }

  }

  describe("scala.concurrent.Future.toJSPromise") {
    import scala.scalajs.js.JSConverters._

    it("basic case") {
      PromiseMock.withMockedPromise { processQueue =>
        implicit val ec = QueueExecutionContext.promises()

        val f = Future { 42 }
        val p = f.toJSPromise
        val pAssertType: js.Promise[Int] = p

        var callbackDone = false

        pAssertType.`then`[Unit] { (x: Int) =>
          expect(x).toEqual(42)
          callbackDone = true
          (): Unit | js.Thenable[Unit]
        }

        processQueue()

        expect(callbackDone).toBeTruthy
      }
    }

    it("Thenable case") {
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
          expect(x).toEqual(42)
          callbackDone = true
          (): Unit | js.Thenable[Unit]
        }

        processQueue()

        expect(callbackDone).toBeTruthy
      }
    }

  }

}
