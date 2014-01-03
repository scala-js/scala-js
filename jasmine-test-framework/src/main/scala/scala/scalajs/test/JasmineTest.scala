/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test

import scala.scalajs.js
import java.util.regex.Pattern
import org.scalajs.jasmine.Jasmine
import org.scalajs.jasmine.JasmineExpectation

class JasmineTest extends Test {
  def describe(name: String)(suite: => Unit): Unit = Jasmine.describe(name, suite _)
  def it(title: String)(test: => Unit): Unit = Jasmine.it(title, test _)
  def xdescribe(name: String)(suite: => Unit): Unit = Jasmine.xdescribe(name, suite _)
  def xit(title: String)(test: => Unit): Unit = Jasmine.xit(title, test _)
  def beforeEach(block: => Unit): Unit = Jasmine.beforeEach(block _)
  def afterEach(block: => Unit): Unit = Jasmine.afterEach(block _)
  def expect(exp: CharSequence): JasmineExpectation = Jasmine.expect(exp.toString)
  def expect(exp: js.Any): JasmineExpectation = Jasmine.expect(exp)
}
