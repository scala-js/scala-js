/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.JSAssert._

class UndefOrTestRequire211 {

  def some[A](v: A): js.UndefOr[A] = v
  def none[A]: js.UndefOr[A] = js.undefined

  // scala.scalajs.js.UndefOr[A]

  @Test def orElse(): Unit = {
    // #2095
    assertEquals("ok", some("ok") orElse "yes")
    assertEquals("yes", none orElse "yes")
  }

}
