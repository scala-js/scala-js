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

package org.scalajs.testsuite.scalalib

import scala.language.implicitConversions

import scala.reflect._
import scala.collection.mutable.ArrayBuilder

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform._

class ArrayBuilderTest {
  import ArrayBuilderTest._

  @noinline
  def erase(x: Any): Any = x

  @inline
  def makeNoInline[T](implicit ct: ClassTag[T]): ArrayBuilder[T] = {
    /* The dance in this method is to be source compatible with the old and
     * new collections. In the new collections, ArrayBuilder.make[T] doesn't
     * take an explicit () parameter list, but it does in the old collections.
     */

    @noinline def ctNoInline = ct

    {
      implicit val ct = ctNoInline
      ArrayBuilder.make[T]
    }
  }

  @inline
  def zerosInline[T: ClassTag](length: Int): Array[T] = {
    val builder = ArrayBuilder.make[T]
    builder.sizeHint(length)
    var i = 0
    while (i < length) {
      builder += null.asInstanceOf[T]
      i += 1
    }
    builder.result()
  }

  @noinline
  def zerosNoInline[T: ClassTag](length: Int): Array[T] =
    zerosInline[T](length)

  @noinline def someInt: Int = 53
  @noinline def someLong: Long = 65
  @noinline def someChar: Char = 'S'
  @noinline def someBoolean: Boolean = false
  @noinline def someString: String = "world"

  @Test def intNormalCaseInline(): Unit = {
    val b = ArrayBuilder.make[Int]
    b += 42
    b += someInt
    val a = b.result()

    assertSame(classOf[Array[Int]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Int])
    assertEquals(42, erase(a(0)))
    assertEquals(53, erase(a(1)))
  }

  @Test def intNormalCaseNoinline(): Unit = {
    val b = makeNoInline[Int]
    b += 42
    b += someInt
    val a = b.result()

    assertSame(classOf[Array[Int]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Int])
    assertEquals(42, erase(a(0)))
    assertEquals(53, erase(a(1)))
  }

  @Test def intZerosInline(): Unit = {
    val a = zerosInline[Int](3)
    assertSame(classOf[Array[Int]], a.getClass)
    assertEquals(3, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Int])
    assertEquals(0, erase(a(0)))
  }

  @Test def intZerosNoinline(): Unit = {
    val a = zerosNoInline[Int](3)
    assertSame(classOf[Array[Int]], a.getClass)
    assertEquals(3, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Int])
    assertEquals(0, erase(a(0)))
  }

  @Test def longNormalCaseInline(): Unit = {
    val b = ArrayBuilder.make[Long]
    b += 42L
    b += someLong
    val a = b.result()

    assertSame(classOf[Array[Long]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Long])
    assertEquals(42L, erase(a(0)))
    assertEquals(65L, erase(a(1)))
  }

  @Test def longNormalCaseNoinline(): Unit = {
    val b = makeNoInline[Long]
    b += 42L
    b += someLong
    val a = b.result()

    assertSame(classOf[Array[Long]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Long])
    assertEquals(42L, erase(a(0)))
    assertEquals(65L, erase(a(1)))
  }

  @Test def longZerosInline(): Unit = {
    val a = zerosInline[Long](3)
    assertSame(classOf[Array[Long]], a.getClass)
    assertEquals(3, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Long])
    assertEquals(0L, erase(a(0)))
  }

  @Test def longZerosNoinline(): Unit = {
    val a = zerosNoInline[Long](3)
    assertSame(classOf[Array[Long]], a.getClass)
    assertEquals(3, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Long])
    assertEquals(0L, erase(a(0)))
  }

  @Test def charNormalCaseInline(): Unit = {
    val b = ArrayBuilder.make[Char]
    b += 'A'
    b += someChar
    val a = b.result()

    assertSame(classOf[Array[Char]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Char])
    assertEquals('A', erase(a(0)))
    assertEquals('S', erase(a(1)))
  }

  @Test def charNormalCaseNoinline(): Unit = {
    val b = makeNoInline[Char]
    b += 'A'
    b += someChar
    val a = b.result()

    assertSame(classOf[Array[Char]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Char])
    assertEquals('A', erase(a(0)))
    assertEquals('S', erase(a(1)))
  }

  @Test def charZerosInline(): Unit = {
    val a = zerosInline[Char](3)
    assertSame(classOf[Array[Char]], a.getClass)
    assertEquals(3, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Char])
    assertEquals('\u0000', erase(a(0)))
  }

  @Test def charZerosNoinline(): Unit = {
    val a = zerosNoInline[Char](3)
    assertSame(classOf[Array[Char]], a.getClass)
    assertEquals(3, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Char])
    assertEquals('\u0000', erase(a(0)))
  }

  @Test def booleanNormalCaseInline(): Unit = {
    val b = ArrayBuilder.make[Boolean]
    b += true
    b += someBoolean
    val a = b.result()

    assertSame(classOf[Array[Boolean]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Boolean])
    assertEquals(true, erase(a(0)))
    assertEquals(false, erase(a(1)))
  }

  @Test def booleanNormalCaseNoinline(): Unit = {
    val b = makeNoInline[Boolean]
    b += true
    b += someBoolean
    val a = b.result()

    assertSame(classOf[Array[Boolean]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Boolean])
    assertEquals(true, erase(a(0)))
    assertEquals(false, erase(a(1)))
  }

  @Test def booleanZerosInline(): Unit = {
    val a = zerosInline[Boolean](3)
    assertSame(classOf[Array[Boolean]], a.getClass)
    assertEquals(3, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Boolean])
    assertEquals(false, erase(a(0)))
  }

  @Test def booleanZerosNoinline(): Unit = {
    val a = zerosNoInline[Boolean](3)
    assertSame(classOf[Array[Boolean]], a.getClass)
    assertEquals(3, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Boolean])
    assertEquals(false, erase(a(0)))
  }

  @Test def unitNormalCaseInline(): Unit = {
    val b = ArrayBuilder.make[Unit]
    b += ()
    val a = b.result()

    assertSame(classOf[Array[Unit]], a.getClass)
    assertEquals(1, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Unit])
    assertEquals((), erase(a(0)))
  }

  @Test def unitNormalCaseNoinline(): Unit = {
    val b = makeNoInline[Unit]
    b += ()
    val a = b.result()

    assertSame(classOf[Array[Unit]], a.getClass)
    assertEquals(1, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Unit])
    assertEquals((), erase(a(0)))
  }

  @Test def unitZerosInline(): Unit = {
    val a = zerosInline[Unit](3)
    assertSame(classOf[Array[Unit]], a.getClass)
    assertEquals(3, a.length)
    if (!executingInJVM) {
      assertTrue(erase(a(0)).isInstanceOf[Unit])
      assertTrue(() == erase(a(0)))
    }
  }

  @Test def unitZerosNoinline(): Unit = {
    val a = zerosNoInline[Unit](3)
    assertSame(classOf[Array[Unit]], a.getClass)
    assertEquals(3, a.length)
    if (!executingInJVM) {
      assertTrue(erase(a(0)).isInstanceOf[Unit])
      assertTrue(() == erase(a(0)))
    }
  }

  @Test def stringNormalCaseInline(): Unit = {
    val b = ArrayBuilder.make[String]
    b += "hello"
    b += someString
    val a = b.result()

    assertSame(classOf[Array[String]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[String])
    assertEquals("hello", erase(a(0)))
    assertEquals("world", erase(a(1)))
  }

  @Test def stringNormalCaseNoinline(): Unit = {
    val b = makeNoInline[String]
    b += "hello"
    b += someString
    val a = b.result()

    assertSame(classOf[Array[String]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[String])
    assertEquals("hello", erase(a(0)))
    assertEquals("world", erase(a(1)))
  }

  @Test def stringZerosInline(): Unit = {
    val a = zerosInline[String](3)
    assertSame(classOf[Array[String]], a.getClass)
    assertEquals(3, a.length)
    assertEquals(null, erase(a(0)))
  }

  @Test def stringZerosNoinline(): Unit = {
    val a = zerosNoInline[String](3)
    assertSame(classOf[Array[String]], a.getClass)
    assertEquals(3, a.length)
    assertEquals(null, erase(a(0)))
  }

  @Test def addAll(): Unit = {
    assumeFalse("Needs at least Scala 2.13",
        scalaVersion.startsWith("2.11.") ||
        scalaVersion.startsWith("2.12."))

    val b = ArrayBuilder.make[Int]
    val arr = Array[Int](1, 2, 3, 4, 5)
    b.addAll(arr, 3, 2)
    assertArrayEquals(Array[Int](4, 5), b.result())
  }
}

object ArrayBuilderTest {
  implicit class ArrayBuilderCompat[A](val __sef: ArrayBuilder[A]) extends AnyVal {
    def addAll(xs: Array[_ <: A], offset: Int, length: Int): Unit =
      throw new AssertionError("unreachable code")
  }
}
