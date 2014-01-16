package org.scalajs.jasmine

import scala.scalajs.js
import java.util.regex.Pattern

trait JasmineExpectation extends js.Object {
  def toBe(exp: js.Any): Unit
  def toEqual(exp: js.Any): Unit
  def toMatch(exp: Pattern): Unit
  // TODO why was there an implementation here?
  def toMatch(exp: String): Unit// = toMatch(Pattern.compile(exp))
  def toBeDefined(): Unit
  def toBeUndefined(): Unit
  def toBeNull(): Unit
  def toBeTruthy(): Unit
  def toBeFalsy(): Unit
  def toContain(exp: js.Any): Unit
  def toBeGreaterThan(exp: js.Number): Unit
  def toBeLessThan(exp: js.Number): Unit
  def toBeCloseTo(exp: js.Number): Unit
  def toThrow(): Unit
  val not: JasmineExpectation
}
