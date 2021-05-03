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

package org.scalajs.testsuite.javalib.util

import org.junit.Assert._
import org.junit.Test

import java.util.Optional
import java.util.function._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class OptionalTest {

  @Test def testCreation(): Unit = {
    Optional.empty[String]()
    Optional.of[String]("")
    assertThrows(classOf[NullPointerException], Optional.of[String](null))
    Optional.ofNullable[String]("")
    Optional.ofNullable[String](null)
  }

  @Test def testEquals(): Unit = {
    assertEquals(Optional.empty[String](), Optional.ofNullable[String](null))
    assertEquals(Optional.of[String](""), Optional.ofNullable[String](""))
    assertNotEquals(Optional.of[String]("1"), Optional.ofNullable[String]("2"))
    assertEquals(Optional.of[Int](1), Optional.ofNullable[Int](1))
    assertNotEquals(Optional.of[Int](1), Optional.ofNullable[Int](2))
    case class Test(value: Long)
    assertEquals(Optional.of(Test(1L)), Optional.ofNullable(Test(1L)))
    assertNotEquals(Optional.of(Test(1L)), Optional.ofNullable(Test(2L)))
  }

  @Test def testIsPresent(): Unit = {
    val emp = Optional.empty[String]()
    assertFalse(emp.isPresent())
    val fullInt = Optional.of[Int](1)
    assertTrue(fullInt.isPresent())
    val fullString = Optional.of[String]("")
    assertTrue(fullString.isPresent())
  }

  @Test def testGet(): Unit = {
    val emp = Optional.empty[String]()
    assertThrows(classOf[NoSuchElementException], emp.get())
    val fullInt = Optional.of[Int](1)
    assertEquals(1, fullInt.get())
    val fullString = Optional.of[String]("")
    assertEquals("", fullString.get())
    class Test()
    val t = new Test()
    assertEquals(t, Optional.of(t).get())
  }

  @Test def testIfPresent(): Unit = {
    val emp = Optional.empty[String]()
    emp.ifPresent(new Consumer[String] {
      def accept(t: String): Unit =
        fail("empty().ifPresent() should not call the callback")
    })

    var result: Option[String] = None
    val fullString = Optional.of("hello")
    fullString.ifPresent(new Consumer[String] {
      def accept(t: String): Unit = {
        assertTrue("of().ifPresent() should call its callback only once", result.isEmpty)
        result = Some(t)
      }
    })
    assertEquals(Some("hello"), result)
  }

  @Test def testFilter(): Unit = {
    assertEquals(Optional.empty[String](),
        Optional.empty[String]().filter(new Predicate[String] {
          def test(t: String): Boolean =
            throw new AssertionError("Optional.empty().filter() should not call its argument")
        }))

    val predicate = new Predicate[String] {
      def test(t: String): Boolean = t.length() < 10
    }

    assertEquals(Optional.empty[String](),
        Optional.of[String]("this string is too long").filter(predicate))
    assertEquals(Optional.of("short"),
        Optional.of("short").filter(predicate))
  }

  @Test def testMap(): Unit = {
    assertEquals(Optional.empty[String](),
        Optional.empty[String]().map[Int](new Function[String, Int] {
          def apply(t: String): Int =
            throw new AssertionError("Optional.empty().map() should not call its argument")
        }))

    val mapper = new Function[String, Int] {
      def apply(t: String): Int = t.length()
    }

    assertEquals(Optional.of(8),
        Optional.of("a string").map[Int](mapper))
    assertEquals(Optional.of(14),
        Optional.of("another string").map[Int](mapper))

    assertEquals(Optional.empty[String](),
        Optional.of("a string").map[String](new Function[String, String] {
          def apply(t: String): String = null
        }))
  }

  @Test def testFlatMap(): Unit = {
    assertEquals(Optional.empty[String](),
        Optional.empty[String]().flatMap[Int](new Function[String, Optional[Int]] {
          def apply(t: String): Optional[Int] =
            throw new AssertionError("Optional.empty().flatMap() should not call its argument")
        }))

    val mapper = new Function[String, Optional[Int]] {
      def apply(t: String): Optional[Int] =
        if (t.isEmpty()) Optional.empty()
        else Optional.of(t.length())
    }

    assertEquals(Optional.of(8),
        Optional.of("a string").flatMap[Int](mapper))
    assertEquals(Optional.of(14),
        Optional.of("another string").flatMap[Int](mapper))
    assertEquals(Optional.empty(),
        Optional.of("").flatMap[Int](mapper))
  }

  @Test def testOrElse(): Unit = {
    val emp = Optional.empty[String]()
    assertEquals("123", emp.orElse("123"))
    val emptyInt = Optional.empty[Int]()
    assertEquals(2, emptyInt.orElse(2))
  }

  @Test def testOrElseGet(): Unit = {
    assertEquals("a string",
        Optional.of("a string").orElseGet(new Supplier[String] {
          def get(): String =
            throw new AssertionError("Optional.of().orElseGet() should not call its argument")
        }))

    assertEquals("fallback",
        Optional.empty[String]().orElseGet(new Supplier[String] {
          def get(): String = "fallback"
        }))

    assertNull(
        Optional.empty[String]().orElseGet(new Supplier[String] {
          def get(): String = null
        }))
  }

  @Test def testOrThrowCustomException(): Unit = {
    assertEquals("a string",
        Optional.of("a string").orElseThrow(new Supplier[IllegalArgumentException] {
          def get(): IllegalArgumentException =
            throw new AssertionError("Optional.of().orElseThrow() should not call its argument")
        }))

    val ex = assertThrows(classOf[IllegalArgumentException],
        Optional.empty[String]().orElseThrow(new Supplier[IllegalArgumentException] {
          def get(): IllegalArgumentException =
            new IllegalArgumentException("fallback")
        }))
    assertEquals("fallback", ex.getMessage())
  }

  @Test def testHashCode(): Unit = {
    val emp = Optional.empty[String]()
    assertEquals(emp.hashCode(), 0)
    val fullString = Optional.of[String]("123")
    assertEquals(fullString.hashCode(), "123".hashCode())
  }

}
