/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Framework    **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jasminetest

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportDescendentObjects

import org.scalajs.jasmine._

@JSExportDescendentObjects
class JasmineTest extends TestSuiteContext {
  def jasmine: JasmineEnv = Jasmine.jasmine
  def describe(name: String)(suite: => Unit): Unit = Jasmine.describe(name, suite _)
  def it(title: String)(test: => Unit): Unit = Jasmine.it(title, test _)
  def xdescribe(name: String)(suite: => Unit): Unit = Jasmine.xdescribe(name, suite _)
  def xit(title: String)(test: => Unit): Unit = Jasmine.xit(title, test _)
  def beforeEach(block: => Unit): Unit = Jasmine.beforeEach(block _)
  def afterEach(block: => Unit): Unit = Jasmine.afterEach(block _)
  def expect(exp: CharSequence): JasmineExpectation =
    Jasmine.expect(if (exp == null) null else exp.toString)
  def expect(exp: js.Any): JasmineExpectation = Jasmine.expect(exp)
  def fail(message: String): Unit = Jasmine.fail(message)
  def runs(block: => Unit): Unit = Jasmine.runs(block _)
  def waits(timeout: Int): Unit = Jasmine.waits(timeout)
  def waitsFor(block: => Boolean, errorMsg: String, timeout: Int): Unit =
    Jasmine.waitsFor(block _, errorMsg, timeout)
}
