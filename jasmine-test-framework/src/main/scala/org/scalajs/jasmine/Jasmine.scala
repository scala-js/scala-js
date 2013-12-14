/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.jasmine

import scala.scalajs.js

object Jasmine extends js.GlobalScope {
  def describe(name: String, suite: js.Function0[_]): Unit = ???
  def it(title: String, test: js.Function0[_]): Unit = ???
  def xdescribe(name: String, suite: js.Function0[_]): Unit = ???
  def xit(title: String, test: js.Function0[_]): Unit = ???
  def beforeEach(block: js.Function0[_]): Unit = ???
  def afterEach(block: js.Function0[_]): Unit = ???
  def expect(exp: js.Any): JasmineExpectation = ???
}
