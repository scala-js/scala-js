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

class OptionalTestOnJDK11 {

  @Test def testIsEmpty(): Unit = {
    val emp = Optional.empty[String]()
    assertTrue(emp.isEmpty())
    val fullInt = Optional.of[Int](1)
    assertFalse(fullInt.isEmpty())
    val fullString = Optional.of[String]("")
    assertFalse(fullString.isEmpty())
  }

  @Test def testIfPresentOrElse(): Unit = {
    var orElseCalled: Boolean = false
    val emp = Optional.empty[String]()
    emp.ifPresentOrElse(new Consumer[String] {
      def accept(t: String): Unit =
        fail("empty().ifPresentOrElse() should not call its first argument")
    }, new Runnable {
      def run(): Unit = {
        assertFalse("empty().ifPresentOrElse() should call its Runnable only once", orElseCalled)
        orElseCalled = true
      }
    })
    assertTrue("empty().ifPresentOrElse() should call its Runnable argument", orElseCalled)

    var result: Option[String] = None
    val fullString = Optional.of("hello")
    fullString.ifPresentOrElse(new Consumer[String] {
      def accept(t: String): Unit = {
        assertTrue("of().ifPresentOrElse() should call its first argument only once", result.isEmpty)
        result = Some(t)
      }
    }, new Runnable {
      def run(): Unit =
        fail("of().ifPresentOrElse() should not call its second argument")
    })
    assertEquals(Some("hello"), result)
  }

  @Test def testOr(): Unit = {
    assertEquals(Optional.of("a string"),
        Optional.of("a string").or(new Supplier[Optional[String]] {
          def get(): Optional[String] =
            throw new AssertionError("Optional.of().or() should not call its argument")
        }))

    assertEquals(Optional.of("fallback"),
        Optional.empty[String]().or(new Supplier[Optional[String]] {
          def get(): Optional[String] = Optional.of("fallback")
        }))

    assertEquals(Optional.empty(),
        Optional.empty[String]().or(new Supplier[Optional[String]] {
          def get(): Optional[String] = Optional.empty()
        }))
  }

  @Test def testOrElseThrow(): Unit = {
    assertEquals("a string",
        Optional.of("a string").orElseThrow())

    assertThrows(classOf[NoSuchElementException],
        Optional.empty[String]().orElseThrow())
  }

}
