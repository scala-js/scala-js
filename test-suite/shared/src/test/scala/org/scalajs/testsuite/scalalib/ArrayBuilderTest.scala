/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013--2015, LAMP/EPFL  **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.scalalib

import scala.language.implicitConversions

import scala.reflect._
import scala.collection.mutable.ArrayBuilder

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform._

class ArrayBuilderTest {

  @noinline
  def erase(x: Any): Any = x

  @inline
  def makeNoInline[T: ClassTag](): ArrayBuilder[T] = {
    @noinline def ct = implicitly[ClassTag[T]]
    ArrayBuilder.make[T]()(ct)
  }

  @inline
  def zerosInline[T: ClassTag](length: Int): Array[T] =
    Array.fill(length)(null.asInstanceOf[T])

  @noinline
  def zerosNoInline[T: ClassTag](length: Int): Array[T] =
    Array.fill(length)(null.asInstanceOf[T])

  @noinline def someInt: Int = 53
  @noinline def someChar: Char = 'S'
  @noinline def someBoolean: Boolean = false
  @noinline def someString: String = "world"

  @Test def Int_normal_case_inline(): Unit = {
    val b = ArrayBuilder.make[Int]()
    b += 42
    b += someInt
    val a = b.result()

    assertSame(classOf[Array[Int]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Int])
    assertEquals(42, erase(a(0)))
    assertEquals(53, erase(a(1)))
  }

  @Test def Int_normal_case_noinline(): Unit = {
    val b = makeNoInline[Int]()
    b += 42
    b += someInt
    val a = b.result()

    assertSame(classOf[Array[Int]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Int])
    assertEquals(42, erase(a(0)))
    assertEquals(53, erase(a(1)))
  }

  @Test def Int_zeros_inline(): Unit = {
    val a = zerosInline[Int](3)
    assertSame(classOf[Array[Int]], a.getClass)
    assertEquals(3, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Int])
    assertEquals(0, erase(a(0)))
  }

  @Test def Int_zeros_noinline(): Unit = {
    val a = zerosNoInline[Int](3)
    assertSame(classOf[Array[Int]], a.getClass)
    assertEquals(3, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Int])
    assertEquals(0, erase(a(0)))
  }

  @Test def Char_normal_case_inline(): Unit = {
    val b = ArrayBuilder.make[Char]()
    b += 'A'
    b += someChar
    val a = b.result()

    assertSame(classOf[Array[Char]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Char])
    assertEquals('A', erase(a(0)))
    assertEquals('S', erase(a(1)))
  }

  @Test def Char_normal_case_noinline(): Unit = {
    val b = makeNoInline[Char]()
    b += 'A'
    b += someChar
    val a = b.result()

    assertSame(classOf[Array[Char]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Char])
    assertEquals('A', erase(a(0)))
    assertEquals('S', erase(a(1)))
  }

  @Test def Char_zeros_inline(): Unit = {
    val a = zerosInline[Char](3)
    assertSame(classOf[Array[Char]], a.getClass)
    assertEquals(3, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Char])
    assertEquals('\0', erase(a(0)))
  }

  @Test def Char_zeros_noinline(): Unit = {
    val a = zerosNoInline[Char](3)
    assertSame(classOf[Array[Char]], a.getClass)
    assertEquals(3, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Char])
    assertEquals('\0', erase(a(0)))
  }

  @Test def Boolean_normal_case_inline(): Unit = {
    val b = ArrayBuilder.make[Boolean]()
    b += true
    b += someBoolean
    val a = b.result()

    assertSame(classOf[Array[Boolean]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Boolean])
    assertEquals(true, erase(a(0)))
    assertEquals(false, erase(a(1)))
  }

  @Test def Boolean_normal_case_noinline(): Unit = {
    val b = makeNoInline[Boolean]()
    b += true
    b += someBoolean
    val a = b.result()

    assertSame(classOf[Array[Boolean]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Boolean])
    assertEquals(true, erase(a(0)))
    assertEquals(false, erase(a(1)))
  }

  @Test def Boolean_zeros_inline(): Unit = {
    val a = zerosInline[Boolean](3)
    assertSame(classOf[Array[Boolean]], a.getClass)
    assertEquals(3, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Boolean])
    assertEquals(false, erase(a(0)))
  }

  @Test def Boolean_zeros_noinline(): Unit = {
    val a = zerosNoInline[Boolean](3)
    assertSame(classOf[Array[Boolean]], a.getClass)
    assertEquals(3, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Boolean])
    assertEquals(false, erase(a(0)))
  }

  @Test def Unit_normal_case_inline(): Unit = {
    val b = ArrayBuilder.make[Unit]()
    b += ()
    val a = b.result()

    assertSame(classOf[Array[Unit]], a.getClass)
    assertEquals(1, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Unit])
    assertEquals((), erase(a(0)))
  }

  @Test def Unit_normal_case_noinline(): Unit = {
    val b = makeNoInline[Unit]()
    b += ()
    val a = b.result()

    assertSame(classOf[Array[Unit]], a.getClass)
    assertEquals(1, a.length)
    assertTrue(erase(a(0)).isInstanceOf[Unit])
    assertEquals((), erase(a(0)))
  }

  @Test def Unit_zeros_inline(): Unit = {
    assumeFalse("Can't run on 2.12.0-M4.",
        System.getProperty("scalac.hasBoxedUnitBug") != null)
    val a = zerosInline[Unit](3)
    assertSame(classOf[Array[Unit]], a.getClass)
    assertEquals(3, a.length)
    if (!executingInJVM) {
      assertTrue(erase(a(0)).isInstanceOf[Unit])
      assertTrue(() == erase(a(0)))
    }
  }

  @Test def Unit_zeros_noinline(): Unit = {
    assumeFalse("Can't run on 2.12.0-M4.",
        System.getProperty("scalac.hasBoxedUnitBug") != null)
    val a = zerosNoInline[Unit](3)
    assertSame(classOf[Array[Unit]], a.getClass)
    assertEquals(3, a.length)
    if (!executingInJVM) {
      assertTrue(erase(a(0)).isInstanceOf[Unit])
      assertTrue(() == erase(a(0)))
    }
  }

  @Test def String_normal_case_inline(): Unit = {
    val b = ArrayBuilder.make[String]()
    b += "hello"
    b += someString
    val a = b.result()

    assertSame(classOf[Array[String]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[String])
    assertEquals("hello", erase(a(0)))
    assertEquals("world", erase(a(1)))
  }

  @Test def String_normal_case_noinline(): Unit = {
    val b = makeNoInline[String]()
    b += "hello"
    b += someString
    val a = b.result()

    assertSame(classOf[Array[String]], a.getClass)
    assertEquals(2, a.length)
    assertTrue(erase(a(0)).isInstanceOf[String])
    assertEquals("hello", erase(a(0)))
    assertEquals("world", erase(a(1)))
  }

  @Test def String_zeros_inline(): Unit = {
    val a = zerosInline[String](3)
    assertSame(classOf[Array[String]], a.getClass)
    assertEquals(3, a.length)
    assertEquals(null, erase(a(0)))
  }

  @Test def String_zeros_noinline(): Unit = {
    val a = zerosNoInline[String](3)
    assertSame(classOf[Array[String]], a.getClass)
    assertEquals(3, a.length)
    assertEquals(null, erase(a(0)))
  }

  @Test def Nothing_and_Null(): Unit = {
    assertSame(classOf[Array[Nothing]], ArrayBuilder.make[Nothing]().result().getClass)
    assertSame(classOf[Array[Null]], ArrayBuilder.make[Null]().result().getClass)

    assertSame(classOf[Array[Nothing]], makeNoInline[Nothing]().result().getClass)
    assertSame(classOf[Array[Null]], makeNoInline[Null]().result().getClass)
  }
}
