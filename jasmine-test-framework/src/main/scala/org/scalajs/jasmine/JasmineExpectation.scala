package org.scalajs.jasmine

import scala.scalajs.js

trait JasmineExpectation extends js.Object {
  def toBe(exp: js.Any): Unit
  def toEqual(exp: js.Any): Unit
  def toMatch(exp: js.RegExp): Unit
  def toMatch(exp: String): Unit
  def toBeDefined(): Unit
  def toBeUndefined(): Unit
  def toBeNull(): Unit
  def toBeTruthy(): Unit
  def toBeFalsy(): Unit
  def toContain(exp: js.Any): Unit
  def toBeGreaterThan(exp: Double): Unit
  def toBeLessThan(exp: Double): Unit
  def toBeCloseTo(exp: Double, precision: Int = 2): Unit
  def toThrow(): Unit
  val not: JasmineExpectation
}
