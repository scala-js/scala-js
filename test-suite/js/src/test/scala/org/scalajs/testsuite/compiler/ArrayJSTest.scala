/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.testsuite.compiler

import scala.scalajs.js

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

class ArrayJSTest {

  private def covariantUpcast[A <: AnyRef](array: Array[_ <: A]): Array[A] =
    array.asInstanceOf[Array[A]]

  @Test
  def setArrayStoreWithJSElems(): Unit = {
    val jsObj = new js.Object
    val obj = new AnyRef
    val str = "foo"
    val list = List(1, 2)
    val jsRangeError = new js.RangeError("foo")

    // Array of JS class
    val a: Array[AnyRef] = covariantUpcast(new Array[js.Object](5))
    a(1) = jsObj
    assertSame(jsObj, a(1))
    a(2) = obj
    assertSame(obj, a(2))
    a(3) = str
    assertEquals(str, a(3))
    a(4) = list
    assertSame(list, a(4))
    a(1) = null
    assertNull(a(1))

    // Array of JS trait
    val b: Array[AnyRef] = covariantUpcast(new Array[js.Iterator[Any]](5))
    b(1) = jsObj
    assertSame(jsObj, b(1))
    b(2) = obj
    assertSame(obj, b(2))
    b(3) = str
    assertEquals(str, b(3))
    b(4) = list
    assertSame(list, b(4))
    b(1) = null
    assertNull(b(1))

    // Array of JS subclass
    val c: Array[js.Object] = covariantUpcast(new Array[js.Error](5))
    c(1) = jsRangeError
    assertSame(jsRangeError, c(1))
    c(2) = jsObj
    assertSame(jsObj, c(2))
    c(3) = str.asInstanceOf[js.Object]
    assertEquals(str, c(3))
    c(1) = null
    assertNull(c(1))
  }

  @Test
  def setArrayStoreExceptionsWithJSElems(): Unit = {
    assumeTrue("Assuming compliant ArrayStores",
        hasCompliantArrayStores)

    val jsObj = new js.Object

    val a: Array[AnyRef] = covariantUpcast(new Array[List[Any]](5))
    assertThrows(classOf[ArrayStoreException], a(1) = jsObj)
  }
}
