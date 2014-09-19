/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

import org.scalajs.jasminetest.JasmineTest

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
    expect(abuf.toJSArray)

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

}
