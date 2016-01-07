package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.utils.AssertThrows._

class ObjectsTestOnJDK7 {

  @Test def equals(): Unit = {
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

  @Test def test_equals(): Unit = {
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
    assertTrue(ju.Objects.deepEquals(Array(Array(1)), Array(Array(1))))
  }

  @Test def test_hashCode(): Unit = {
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

  @Test def test_toString(): Unit = {
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

  @Test def requireNonNull(): Unit = {
    assertThrows(classOf[NullPointerException], ju.Objects.requireNonNull(null))
    assertThrows(classOf[NullPointerException], ju.Objects.requireNonNull(null, "message"))
    assertEquals("abc", ju.Objects.requireNonNull("abc"))
    assertEquals("abc", ju.Objects.requireNonNull("abc", ""))
  }
}
