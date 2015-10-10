package org.scalajs.jasmine

import scala.scalajs.js

@js.native
object Jasmine extends js.GlobalScope {
  def jasmine: JasmineEnv = js.native
  def describe(name: String, suite: js.Function0[_]): Unit = js.native
  def it(title: String, test: js.Function0[_]): Unit = js.native
  def xdescribe(name: String, suite: js.Function0[_]): Unit = js.native
  def xit(title: String, test: js.Function0[_]): Unit = js.native
  def beforeEach(block: js.Function0[_]): Unit = js.native
  def afterEach(block: js.Function0[_]): Unit = js.native
  def expect(exp: js.Any): JasmineExpectation = js.native
  def fail(message: String): Unit = js.native
  def runs(block: js.Function0[_]): Unit = js.native
  def waits(timeout: Int): Unit = js.native
  def waitsFor(block: js.Function0[Boolean], errorMsg: String, timeout: Int): Unit = js.native
}
