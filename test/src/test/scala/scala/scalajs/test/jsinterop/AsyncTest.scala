/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package jsinterop

import scala.scalajs.js
import scala.scalajs.test.JasmineTest

import scala.concurrent.{Future, ExecutionContext}
import scala.scalajs.concurrent.JSExecutionContext

import scala.collection.mutable.ArrayBuffer

import org.scalajs.jasmine.JasmineExpectation

object AsyncTest extends JasmineTest {

  def asyncTest(implicit executor: ExecutionContext) = {
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
    expect(abuf.toArray)

  describe("scala.scalajs.concurrent.JSExecutionContext.queue") {

    beforeEach {
      jasmine.Clock.useMock()
    }

    it("should correctly order future calls") {
      val res = asyncTest(JSExecutionContext.queue)

      expect(res).toEqual(js.Array(
        "prep-future",
        "prep-map",
        "prep-foreach",
        "done"))

      jasmine.Clock.tick(1)

      expect(res).toEqual(js.Array(
        "prep-future",
        "prep-map",
        "prep-foreach",
        "done",
        "future",
        "map",
        "foreach"))
    }

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

}
