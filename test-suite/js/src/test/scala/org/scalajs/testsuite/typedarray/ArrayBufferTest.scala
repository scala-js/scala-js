/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.typedarray

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.Requires

import scala.scalajs.js.typedarray._

object ArrayBufferTest extends Requires.TypedArray

class ArrayBufferTest {

  @Test def lengthConstructor(): Unit = {
    val x = new ArrayBuffer(100)
    assertTrue(x.isInstanceOf[ArrayBuffer])
    assertEquals(100, x.byteLength)
  }

  @Test def slice_with_one_arg(): Unit = {
    val x = new ArrayBuffer(100)
    val y = x.slice(10)
    assertEquals(90, y.byteLength)

  }

  @Test def slice_with_two_args(): Unit = {
    val x = new ArrayBuffer(100)
    val y = x.slice(10, 20)
    assertEquals(10, y.byteLength)
  }
}
