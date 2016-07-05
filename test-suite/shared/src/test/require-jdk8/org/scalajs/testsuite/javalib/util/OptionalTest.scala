/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import org.junit.Assert._
import org.junit.Test

import java.util.Optional

import org.scalajs.testsuite.utils.AssertThrows._

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

  @Test def testOrElse(): Unit = {
    val emp = Optional.empty[String]()
    assertEquals("123", emp.orElse("123"))
    val emptyInt = Optional.empty[Int]()
    assertEquals(2, emptyInt.orElse(2))
  }

  @Test def testHashCode(): Unit = {
    val emp = Optional.empty[String]()
    assertEquals(emp.hashCode(), 0)
    val fullString = Optional.of[String]("123")
    assertEquals(fullString.hashCode(), "123".hashCode())
  }

}
