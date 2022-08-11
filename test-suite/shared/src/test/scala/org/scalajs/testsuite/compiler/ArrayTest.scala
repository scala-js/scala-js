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

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

class ArrayTest {

  private def covariantUpcast[A <: AnyRef](array: Array[_ <: A]): Array[A] =
    array.asInstanceOf[Array[A]]

  @Test
  def getArrayIndexOutOfBounds(): Unit = {
    assumeTrue("Assuming compliant ArrayIndexOutOfBounds",
        hasCompliantArrayIndexOutOfBounds)

    val a = new Array[Int](5)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], a(-1))
    assertThrows(classOf[ArrayIndexOutOfBoundsException], a(5))
    assertThrows(classOf[ArrayIndexOutOfBoundsException], a(Int.MinValue))
    assertThrows(classOf[ArrayIndexOutOfBoundsException], a(Int.MaxValue))

    val b = new Array[AnyRef](5)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], b(-1))
    assertThrows(classOf[ArrayIndexOutOfBoundsException], b(5))
    assertThrows(classOf[ArrayIndexOutOfBoundsException], b(Int.MinValue))
    assertThrows(classOf[ArrayIndexOutOfBoundsException], b(Int.MaxValue))

    val c = new Array[Seq[_]](5)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], c(-1))
    assertThrows(classOf[ArrayIndexOutOfBoundsException], c(5))
    assertThrows(classOf[ArrayIndexOutOfBoundsException], c(Int.MinValue))
    assertThrows(classOf[ArrayIndexOutOfBoundsException], c(Int.MaxValue))
  }

  @Test
  def setArrayIndexOutOfBounds(): Unit = {
    assumeTrue("Assuming compliant ArrayIndexOutOfBounds",
        hasCompliantArrayIndexOutOfBounds)

    val a = new Array[Int](5)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], a(-1) = 1)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], a(5) = 1)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], a(Int.MinValue) = 1)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], a(Int.MaxValue) = 1)

    val b = new Array[AnyRef](5)
    val obj = new AnyRef
    assertThrows(classOf[ArrayIndexOutOfBoundsException], b(-1) = obj)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], b(5) = obj)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], b(Int.MinValue) = obj)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], b(Int.MaxValue) = obj)

    val c = new Array[Seq[_]](5)
    val seq = List(1, 2)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], c(-1) = seq)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], c(5) = seq)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], c(Int.MinValue) = seq)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], c(Int.MaxValue) = seq)

    /* IndexOutOfBoundsException is stronger than ArrayStoreException
     * (whether the latter is compliant or not).
     */
    val d: Array[AnyRef] = covariantUpcast(c)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], d(-1) = obj)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], d(5) = obj)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], d(Int.MinValue) = obj)
    assertThrows(classOf[ArrayIndexOutOfBoundsException], d(Int.MaxValue) = obj)
  }

  @Test
  def setArrayStoreExceptions(): Unit = {
    assumeTrue("Assuming compliant ArrayStores",
        hasCompliantArrayStores)

    val obj = new AnyRef
    val str = "foo"
    val list = List(1, 2)
    val vector = Vector(3, 4)

    val a: Array[AnyRef] = covariantUpcast(new Array[Seq[_]](5))
    a(1) = list
    assertSame(list, a(1))
    assertThrows(classOf[ArrayStoreException], a(1) = obj)
    assertSame(list, a(1))
    assertThrows(classOf[ArrayStoreException], a(2) = str)
    assertNull(a(2))
    a(3) = vector
    assertSame(vector, a(3))
    a(1) = null
    assertNull(a(1))

    val b: Array[Seq[_]] = covariantUpcast(new Array[List[Any]](5))
    b(1) = list
    assertSame(list, b(1))
    assertThrows(classOf[ArrayStoreException], b(1) = vector)
    assertSame(list, b(1))

    val c: Array[Number] = covariantUpcast(new Array[Integer](5))
    c(1) = Integer.valueOf(5)
    assertEquals(5, c(1))
    assertThrows(classOf[ArrayStoreException], c(1) = java.lang.Double.valueOf(5.5))
    assertEquals(5, c(1))
    if (executingInJVM) {
      assertThrows(classOf[ArrayStoreException], c(2) = java.lang.Double.valueOf(5.0))
      assertNull(c(2))
    } else {
      c(2) = java.lang.Double.valueOf(5.0)
      assertEquals(5.0, c(2))
    }
    val c2: Array[Object] = covariantUpcast(c)
    c2(3) = Integer.valueOf(42)
    assertThrows(classOf[ArrayStoreException], c2(3) = str)
    assertEquals(42, c2(3))
    assertEquals(42, c(3))

    val x: Array[AnyRef] = covariantUpcast(new Array[Array[Seq[_]]](5))
    x(1) = new Array[Seq[_]](1)
    x(2) = new Array[List[Any]](1)
    assertThrows(classOf[ArrayStoreException], x(3) = new Array[String](1))
    assertThrows(classOf[ArrayStoreException], x(3) = new Array[AnyRef](1))
    assertThrows(classOf[ArrayStoreException], x(3) = new Array[Int](1))
    assertThrows(classOf[ArrayStoreException], x(3) = obj)
    assertThrows(classOf[ArrayStoreException], x(3) = str)
    x(1) = null
    assertNull(x(1))

    val y: Array[AnyRef] = covariantUpcast(new Array[Array[Int]](5))
    y(1) = new Array[Int](1)
    assertThrows(classOf[ArrayStoreException], y(3) = new Array[String](1))
    assertThrows(classOf[ArrayStoreException], y(3) = new Array[AnyRef](1))
    assertThrows(classOf[ArrayStoreException], y(3) = new Array[List[Any]](1))
    assertThrows(classOf[ArrayStoreException], y(3) = obj)
    assertThrows(classOf[ArrayStoreException], y(3) = str)
    y(1) = null
    assertNull(y(1))
  }

  @Test
  def arraySelectSideEffecting_Issue3848(): Unit = {
    assumeTrue("Assuming compliant ArrayIndexOutOfBounds",
        hasCompliantArrayIndexOutOfBounds)

    // Force unit return type so the Emitter tries to get rid of the expression.
    @noinline
    def testAccess(a: Array[Int]): Unit = a(1)

    assertThrows(classOf[ArrayIndexOutOfBoundsException], testAccess(Array()))
  }
}
