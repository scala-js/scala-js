package org.scalajs.jasmine

import scala.scalajs.js

@js.native
trait JasmineExpectation extends js.Object {
  def toBe(exp: js.Any): Unit = js.native
  def toEqual(exp: js.Any): Unit = js.native
  def toMatch(exp: js.RegExp): Unit = js.native
  def toMatch(exp: String): Unit = js.native
  def toBeDefined(): Unit = js.native
  def toBeUndefined(): Unit = js.native
  def toBeNull(): Unit = js.native
  def toBeTruthy(): Unit = js.native
  def toBeFalsy(): Unit = js.native
  def toContain(exp: js.Any): Unit = js.native
  def toBeGreaterThan(exp: Double): Unit = js.native
  def toBeLessThan(exp: Double): Unit = js.native
  def toBeCloseTo(exp: Double, precision: Int = 2): Unit = js.native
  def toThrow(): Unit = js.native
  val not: JasmineExpectation = js.native
}
