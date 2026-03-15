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

import java.{util => ju}

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows.{assertThrows, _}
import org.scalajs.testsuite.utils.Platform.hasCompliantNullPointers

class ObjectsTest {

  @noinline private def hide[T](x: T): T = x

  @Test def testEquals(): Unit = {
    val obj = new Object
    assertTrue(ju.Objects.equals(null, null))
    assertFalse(ju.Objects.equals(null, obj))
    assertFalse(ju.Objects.equals(obj, null))
    assertTrue(ju.Objects.equals(obj, obj))
    assertFalse(ju.Objects.equals(new Object, new Object))
    assertTrue(ju.Objects.equals(1, 1))
    assertFalse(ju.Objects.equals(1, 2))
    assertTrue(ju.Objects.equals("abc", "abc"))
    assertFalse(ju.Objects.equals("abc", "abd"))
  }

  @Test def testDeepEquals(): Unit = {
    val obj = new Object
    assertTrue(ju.Objects.deepEquals(null, null))
    assertFalse(ju.Objects.deepEquals(null, obj))
    assertFalse(ju.Objects.deepEquals(obj, null))
    assertTrue(ju.Objects.deepEquals(obj, obj))
    assertFalse(ju.Objects.deepEquals(new Object, new Object))
    assertTrue(ju.Objects.deepEquals(1, 1))
    assertFalse(ju.Objects.deepEquals(1, 2))
    assertTrue(ju.Objects.deepEquals("abc", "abc"))
    assertFalse(ju.Objects.deepEquals("abc", "abd"))
    assertFalse(ju.Objects.deepEquals(0.0, -0.0))
    assertTrue(ju.Objects.deepEquals(0.0, 0.0))
    assertTrue(ju.Objects.deepEquals(Double.NaN, Double.NaN))
    assertTrue(ju.Objects.deepEquals(Array(Array(1)), Array(Array(1))))
  }

  @Test def testHashCode(): Unit = {
    val obj = new Object
    assertEquals(0, ju.Objects.hashCode(null))
    assertEquals(obj.hashCode, ju.Objects.hashCode(obj))
    assertEquals(1.hashCode, ju.Objects.hashCode(1))
  }

  @Test def hash(): Unit = {
    assertEquals(ju.Arrays.hashCode(Array.empty[AnyRef]), ju.Objects.hash())
    assertEquals(ju.Arrays.hashCode(Array[AnyRef](null)), ju.Objects.hash(null))
    assertEquals(ju.Arrays.hashCode(Array[AnyRef]("1")), ju.Objects.hash("1"))
    assertEquals(ju.Arrays.hashCode(Array[AnyRef]("1", "2")), ju.Objects.hash("1", "2"))
    assertEquals(ju.Arrays.hashCode(Array[AnyRef]("1", null)), ju.Objects.hash("1", null))
  }

  @Test def testToString(): Unit = {
    val obj = new Object
    assertEquals("null", ju.Objects.toString(null))
    assertEquals("abc", ju.Objects.toString(null, "abc"))
    assertEquals(obj.toString, ju.Objects.toString(obj))
    assertEquals(obj.toString, ju.Objects.toString(obj, "abc"))
    assertEquals(1.toString, ju.Objects.toString(1))
    assertEquals(1.toString, ju.Objects.toString(1, "abc"))
  }

  @Test def compare(): Unit = {
    val cmp1: ju.Comparator[Int] = Ordering[Int]
    val cmp2: ju.Comparator[AnyRef] = new Ordering[AnyRef] {
      def compare(x: AnyRef, y: AnyRef): Int =
        x.hashCode.compareTo(y.hashCode)
    }
    assertEquals(0, ju.Objects.compare(null, null, cmp2))
    assertEquals(0, ju.Objects.compare(1, 1, cmp1))
    assertTrue(ju.Objects.compare(2, 1, cmp1) > 0)
    assertTrue(ju.Objects.compare(1, 2, cmp1) < 0)
  }

  /* The overloads of requireNonNull are subject to intrinsic optimizations.
   * Make sure to test them with arguments that are both known and not-known
   * to be nullable or non-nullable.
   */

  @Test def requireNonNull(): Unit = {
    assertThrowsNPEIfCompliant(ju.Objects.requireNonNull(null))
    assertThrowsNPEIfCompliant(ju.Objects.requireNonNull(hide[String](null)))

    assertEquals("abc", ju.Objects.requireNonNull("abc"))
    assertEquals("abc", ju.Objects.requireNonNull(hide[String]("abc")))
  }

  @Test def requireNonNullWithMessage(): Unit = {
    if (hasCompliantNullPointers) {
      val e1 = assertThrows(classOf[NullPointerException],
          ju.Objects.requireNonNull(null, "the message"))
      assertEquals("the message", e1.getMessage())

      val e2 = assertThrows(classOf[NullPointerException],
          ju.Objects.requireNonNull(hide[String](null), "the message"))
      assertEquals("the message", e2.getMessage())
    }

    assertEquals("abc", ju.Objects.requireNonNull("abc", "unexpected"))
    assertEquals("abc", ju.Objects.requireNonNull(hide[String]("abc"), "unexpected"))

    // The effects of computing the arguments are preserved, in order

    locally {
      var effects = 5

      assertThrows(classOf[IllegalStateException], {
        ju.Objects.requireNonNull({
          effects *= 2
          hide[String](null)
        }, {
          effects += 1
          throw new IllegalStateException()
          hide[String]("unexpected")
        })
      })

      assertEquals(11, effects)
    }

    if (hasCompliantNullPointers) {
      var effects = 5

      assertThrows(classOf[NullPointerException], {
        ju.Objects.requireNonNull({
          effects *= 2
          hide[String](null)
        }, {
          effects += 1
          hide[String]("unexpected")
        })
      })

      assertEquals(11, effects)
    }
  }

  @Test def requireNonNullWithMsgSupplier(): Unit = {
    val message = "All is well!"

    val successSupplier = new ju.function.Supplier[String] {
      def get(): String = message
    }

    // Hidden version; with a distinct instance so that successSupplier can still be inlined
    val hiddenSuccessSupplier = hide(new ju.function.Supplier[String] {
      def get(): String = message
    })

    val failureSupplier = new ju.function.Supplier[String] {
      def get(): String = {
        throw new AssertionError(
            "Objects.requireNonNull() should not have called Supplier")
      }
    }

    if (hasCompliantNullPointers) {
      val e1 = assertThrows(classOf[NullPointerException],
          ju.Objects.requireNonNull(null, successSupplier))
      assertEquals(message, e1.getMessage())
      val e2 = assertThrows(classOf[NullPointerException],
          ju.Objects.requireNonNull(hide[String](null), successSupplier))
      assertEquals(message, e2.getMessage())

      val e3 = assertThrows(classOf[NullPointerException],
          ju.Objects.requireNonNull(null, hiddenSuccessSupplier))
      assertEquals(message, e3.getMessage())
      val e4 = assertThrows(classOf[NullPointerException],
          ju.Objects.requireNonNull(hide[String](null), hiddenSuccessSupplier))
      assertEquals(message, e4.getMessage())

      // If the supplier returns a null message, we get a null message
      val e5 = assertThrows(classOf[NullPointerException],
          ju.Objects.requireNonNull(null, () => null))
      assertNull(e5.getMessage())
      val e6 = assertThrows(classOf[NullPointerException],
          ju.Objects.requireNonNull(hide[String](null), () => null))
      assertNull(e6.getMessage())

      // If the supplier itself is null as well, we get an NPE with an unspecified message
      assertThrows(classOf[NullPointerException],
          ju.Objects.requireNonNull(null, null: ju.function.Supplier[String]))
      assertThrows(classOf[NullPointerException],
          ju.Objects.requireNonNull(hide[String](null), null: ju.function.Supplier[String]))
      assertThrows(classOf[NullPointerException],
          ju.Objects.requireNonNull(null, hide[ju.function.Supplier[String]](null)))
      assertThrows(classOf[NullPointerException],
          ju.Objects.requireNonNull(hide[String](null), hide[ju.function.Supplier[String]](null)))
    }

    assertEquals("abc", ju.Objects.requireNonNull("abc", failureSupplier))
    assertEquals("abc", ju.Objects.requireNonNull(hide[String]("abc"), failureSupplier))

    assertEquals("abc",
        ju.Objects.requireNonNull("abc", null: ju.function.Supplier[String]))
    assertEquals("abc",
        ju.Objects.requireNonNull(hide[String]("abc"), hide[ju.function.Supplier[String]](null)))

    // The effects of computing the arguments are preserved, in order

    locally {
      var effects = 5

      assertThrows(classOf[IllegalStateException], {
        ju.Objects.requireNonNull({
          effects *= 2
          hide[String](null)
        }, {
          effects += 1
          throw new IllegalStateException()
          hide[ju.function.Supplier[String]](null)
        })
      })

      assertEquals(11, effects)
    }

    if (hasCompliantNullPointers) {
      var effects = 5

      assertThrows(classOf[NullPointerException], {
        ju.Objects.requireNonNull({
          effects *= 2
          hide[String](null)
        }, {
          effects += 1

          { () =>
            effects = -100 - effects
            "the message"
          }: ju.function.Supplier[String]
        })
      })

      assertEquals(-111, effects)
    }
  }

  @Test def isNull(): Unit = {
    assertTrue(ju.Objects.isNull(null))
    assertFalse(ju.Objects.isNull(new Object))
  }

  @Test def nonNull(): Unit = {
    assertFalse(ju.Objects.nonNull(null))
    assertTrue(ju.Objects.nonNull(new Object))
  }
}
