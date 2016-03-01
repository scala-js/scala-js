package org.scalajs.testsuite.junit

import org.junit.Test

import org.junit.Assert._
import org.hamcrest.CoreMatchers._

import org.scalajs.testsuite.utils.AssertThrows._

import scala.util.{Failure, Success, Try}

class JUnitAssertionsTest {
  private final val NotEquals = false
  private final val ShallNotPass = false

  private def testIfAsserts(assertion: => Unit, shouldPass: Boolean = true): Unit = {
    Try(assertion) match {
      case Success(_) =>
        if (!shouldPass)
          fail("Assertion should have failed.")

      case Failure(assErr: AssertionError) =>
        if (shouldPass) {
          val msg = "Assertion should not have failed."
          throw new AssertionError(msg).initCause(assErr)
        }

      case Failure(ex) => throw ex // Unexpected exception, let bubble up.
    }
  }

  @Test
  def testAssertTrueFalse(): Unit = {
    testIfAsserts(assertTrue("'true' did not assertTrue", true))
    testIfAsserts(assertTrue(true))

    testIfAsserts(assertFalse("'false' did not assertFalse", false))
    testIfAsserts(assertFalse(false))

    testIfAsserts(assertTrue("'true' did not assertTrue", false), ShallNotPass)
    testIfAsserts(assertTrue(false), ShallNotPass)

    testIfAsserts(assertFalse("'false' did not assertFalse", true), ShallNotPass)
    testIfAsserts(assertFalse(true), ShallNotPass)
  }

  @Test
  def testAssertNull(): Unit = {
    testIfAsserts(assertNull("'null' did not assertNull", null))
    testIfAsserts(assertNull(null))

    testIfAsserts(assertNotNull("'new Object' did not assertNotNull", new Object))
    testIfAsserts(assertNotNull(new Object))

    testIfAsserts(assertNull("'null' did not assertNull", new Object), ShallNotPass)
    testIfAsserts(assertNull(new Object), ShallNotPass)

    testIfAsserts(assertNotNull("'null' did not assertNotNull", null), ShallNotPass)
    testIfAsserts(assertNotNull(null), ShallNotPass)
  }

  @Test
  def testAssertSame(): Unit = {
    // Setup
    val obj = new Object()
    val str = "abcd"
    val nullRef: AnyRef = null

    def testAssertion(expected: AnyRef, actual: AnyRef, equals: Boolean = true): Unit = {
      testIfAsserts(assertSame("References where not equal", expected, actual), equals)
      testIfAsserts(assertSame(expected, actual), equals)
      testIfAsserts(assertNotSame("References where equal", expected, actual), !equals)
      testIfAsserts(assertNotSame(expected, actual), !equals)
    }

    // Tests
    testAssertion(obj, obj)
    testAssertion(str, str)
    testAssertion(nullRef, nullRef)

    testAssertion(new Object, new Object, NotEquals)
  }

  @Test
  def testAssertEquals(): Unit = {

    // Setup
    val obj = new Object()
    val str = "abcd"
    val nullRef: AnyRef = null

    // Object equality tests
    def testAssertion(expected: AnyRef, actual: AnyRef, equals: Boolean = true): Unit = {
      testIfAsserts(assertEquals(s"Asserting $expected == $actual", expected, actual), equals)
      testIfAsserts(assertEquals(expected, actual), equals)
      testIfAsserts(assertNotEquals(s"Asserting $expected != $actual", expected, actual), !equals)
      testIfAsserts(assertNotEquals(expected, actual), !equals)
    }

    testAssertion(nullRef, nullRef)
    testAssertion(obj, obj)
    testAssertion(str, str)
    testAssertion(new Object, null, NotEquals)
    testAssertion(null, new Object, NotEquals)
    testAssertion(new Object, new Object, NotEquals)

    testAssertion("", "")
    testAssertion("42", "42")
    testAssertion("asdfasfsafs", "asdfasfsafs")
    testAssertion(List(1, 2, 3), List(1, 2, 3))
    testAssertion(List(1L, 2L, 3L), List(1L, 2L, 3L))
    testAssertion(Vector(1L, 2L, 3L), List(1L, 2L, 3L))
    testAssertion((1, 2, 3), (1, 2, 3))
    testAssertion("", "d", equals = false)
    testAssertion("42", "1", equals = false)
    testAssertion("asdfasfsafs", "asdfuhafs", NotEquals)
    testAssertion(List(1, 2, 3), List(1, 2, 4), NotEquals)
    testAssertion(List(1L, 2L, 3L), List(1L, 2L, 4L), NotEquals)
    testAssertion(Vector(1L, 2L, 3L), List(1L, 2L, 4L), NotEquals)
    testAssertion((1, 2, 3), (1, 2, 4), NotEquals)
  }

  @Test
  def testAssertEqualsByte(): Unit = {
    def testByteAssertion(expected: Byte, actual: Byte, equals: Boolean = true): Unit = {
      testIfAsserts(assertEquals(s"Asserting $expected == $actual", expected, actual), equals)
      testIfAsserts(assertEquals(expected, actual), equals)
      testIfAsserts(assertNotEquals(s"Asserting $expected != $actual", expected, actual), !equals)
      testIfAsserts(assertNotEquals(expected, actual), !equals)
    }
    testByteAssertion(0, 0)
    testByteAssertion(42, 42)
    testByteAssertion(-42, -42)
    testByteAssertion(Byte.MinValue, Byte.MinValue)
    testByteAssertion(Byte.MaxValue, Byte.MaxValue)
    testByteAssertion(1, 2, NotEquals)

  }

  @Test
  def testAssertEqualsChar(): Unit = {
    def testCharAssertion(expected: Char, actual: Char, equals: Boolean = true): Unit = {
      testIfAsserts(assertEquals(s"Asserting $expected == $actual", expected, actual), equals)
      testIfAsserts(assertEquals(expected, actual), equals)
      testIfAsserts(assertNotEquals(s"Asserting $expected != $actual", expected, actual), !equals)
      testIfAsserts(assertNotEquals(expected, actual), !equals)
    }

    testCharAssertion('a', 'a')
    testCharAssertion('@', '@')
    testCharAssertion('\n', '\n')
    testCharAssertion('a', '\0', NotEquals)
    testCharAssertion('a', '@', NotEquals)
    testCharAssertion('a', '\n', NotEquals)
  }

  @Test
  def testAssertEqualsShort(): Unit = {
    def testShortAssertion(expected: Short, actual: Short, equals: Boolean = true): Unit = {
      testIfAsserts(assertEquals(s"Asserting $expected == $actual", expected, actual), equals)
      testIfAsserts(assertEquals(expected, actual), equals)
      testIfAsserts(assertNotEquals(s"Asserting $expected != $actual", expected, actual), !equals)
      testIfAsserts(assertNotEquals(expected, actual), !equals)
    }
    testShortAssertion(0, 0)
    testShortAssertion(42, 42)
    testShortAssertion(-42, -42)
    testShortAssertion(Short.MinValue, Short.MinValue)
    testShortAssertion(Short.MaxValue, Short.MaxValue)
    testShortAssertion(1, 2, NotEquals)
  }

  @Test
  def testAssertEqualsInt(): Unit = {
    def testIntAssertion(expected: Int, actual: Int, equals: Boolean = true): Unit = {
      testIfAsserts(assertEquals(s"Asserting $expected == $actual", expected, actual), equals)
      testIfAsserts(assertEquals(expected, actual), equals)
      testIfAsserts(assertNotEquals(s"Asserting $expected != $actual", expected, actual), !equals)
      testIfAsserts(assertNotEquals(expected, actual), !equals)
    }

    testIntAssertion(0, 0)
    testIntAssertion(42, 42)
    testIntAssertion(-42, -42)
    testIntAssertion(Int.MinValue, Int.MinValue)
    testIntAssertion(Int.MaxValue, Int.MaxValue)
    testIntAssertion(1, 2, NotEquals)
  }

  @Test
  def testAssertEqualsLong(): Unit = {
    def testLongAssertion(expected: Long, actual: Long, equals: Boolean = true): Unit = {
      testIfAsserts(assertEquals(s"Asserting $expected == $actual", expected, actual), equals)
      testIfAsserts(assertEquals(expected, actual), equals)
      testIfAsserts(assertNotEquals(s"Asserting $expected != $actual", expected, actual), !equals)
      testIfAsserts(assertNotEquals(expected, actual), !equals)
    }

    testLongAssertion(0L, 0L)
    testLongAssertion(42L, 42L)
    testLongAssertion(-42L, -42L)
    testLongAssertion(Long.MinValue, Long.MinValue)
    testLongAssertion(Long.MaxValue, Long.MaxValue)
    testLongAssertion(1L, 2L, NotEquals)
  }

  @Test
  def testAssertEqualsDouble(): Unit = {
    def testDoubleAssertion(expected: Double, actual: Double, delta: Double, equals: Boolean = true): Unit = {
      testIfAsserts(assertEquals(s"Asserting $expected == $actual", expected, actual, delta), equals)
      testIfAsserts(assertEquals(expected, actual, delta), equals)
      testIfAsserts(assertNotEquals(s"Asserting $expected != $actual", expected, actual, delta), !equals)
      testIfAsserts(assertNotEquals(expected, actual, delta), !equals)
    }

    testDoubleAssertion(1d, 1d, 0d)
    testDoubleAssertion(1d, 2d, 1d)
    testDoubleAssertion(1d, 2d, 10d)
    testDoubleAssertion(1d, 1.1d, 0.2d)
    testDoubleAssertion(1d, 2d, 0d, NotEquals)
    testDoubleAssertion(1d, 2d, 0.5d, NotEquals)
//    testDoubleAssertion(Double.NegativeInfinity, Double.NegativeInfinity, 1.0d)
  }

  @Test
  def testAssertEqualsFloat(): Unit = {
    def testFloatAssertion(expected: Float, actual: Float, delta: Float,
        equals: Boolean = true): Unit = {
      testIfAsserts(assertEquals(s"Asserting $expected == $actual", expected, actual, delta), equals)
      testIfAsserts(assertEquals(expected, actual, delta), equals)
      testIfAsserts(assertNotEquals(s"Asserting $expected != $actual", expected, actual, delta), !equals)
      testIfAsserts(assertNotEquals(expected, actual, delta), !equals)
    }

    testFloatAssertion(1f, 1f, 0f)
    testFloatAssertion(1f, 2f, 1f)
    testFloatAssertion(1f, 2f, 10f)
    testFloatAssertion(1f, 1.1f, 0.2f)
    testFloatAssertion(1f, 2f, 0f, NotEquals)
    testFloatAssertion(1f, 2f, 0.5f, NotEquals)
  }

  @Test
  def testAssertArrayEquals(): Unit = {
    // setup
    val (obj1, obj2): (AnyRef, AnyRef) = ("0", "1")
    val arr1 = Array(obj1)

    val message = "Should be different up to != operator"

    def testAnyRefAssertion(expected: Array[AnyRef], actual: Array[AnyRef],
                            equals: Boolean = true): Unit = {
      testIfAsserts(assertArrayEquals(message, expected, actual), equals)
      testIfAsserts(assertArrayEquals(expected, actual), equals)
    }
    def testIntAssertion(expected: Array[Int], actual: Array[Int],
                         equals: Boolean = true): Unit = {
      testIfAsserts(assertArrayEquals(message, expected, actual), equals)
      testIfAsserts(assertArrayEquals(expected, actual), equals)
    }
    def testLongAssertion(expected: Array[Long], actual: Array[Long],
                          equals: Boolean = true): Unit = {
      testIfAsserts(assertArrayEquals(message, expected, actual), equals)
      testIfAsserts(assertArrayEquals(expected, actual), equals)
    }


    // Array tests
    testAnyRefAssertion(arr1, arr1)
    testAnyRefAssertion(Array(obj1), Array(obj1))
    testAnyRefAssertion(Array(obj1, obj2, obj2), Array(obj1, obj2, obj2))
    testAnyRefAssertion(Array(obj1), Array("0"))
    testAnyRefAssertion(Array(Array(1), Array(2, Array(3))),
        Array(Array(1), Array(2, Array(3))))
    testIntAssertion(Array(1, 2, 3), Array(1, 2, 3))
    testLongAssertion(Array(1L, 2L, 3L), Array(1L, 2L, 3L))

    testAnyRefAssertion(Array(obj1), Array(obj2), NotEquals)
    testAnyRefAssertion(Array(obj1, obj2, obj2), Array(obj1, obj2, obj1), NotEquals)
    testAnyRefAssertion(Array(obj1), Array("4"), NotEquals)
    testAnyRefAssertion(Array(Array(2), Array(2, Array(3))), Array(Array(1),
        Array(2, Array(3))), NotEquals)
    testAnyRefAssertion(Array(Array(1, 2), Array(2, Array(3))),
        Array(Array(1), Array(2, Array(3))), NotEquals)
    testAnyRefAssertion(Array(Array(1), Array(2, Array(3))),
        Array(Array(1, 4), Array(2, Array(3))), NotEquals)
    testIntAssertion(Array(1, 2, 3), Array(1, 3, 3), NotEquals)
    testLongAssertion(Array(1L, 2L, 3L), Array(1L, 1L, 3L), NotEquals)
  }

  @Test
  def testAssertArrayEqualsDouble(): Unit = {
    def testDoubleAssertion(expected: Array[Double], actual: Array[Double],
        delta: Double, equals: Boolean = true): Unit = {
      val message = "Should be different up to != operator"
      testIfAsserts(assertArrayEquals(message, expected, actual, delta), equals)
      testIfAsserts(assertArrayEquals(expected, actual, delta), equals)
    }

    testDoubleAssertion(Array(1d, 2d, 3d), Array(1d, 2d, 4d), 1d)
    testDoubleAssertion(Array(1d, 2d, 3d), Array(1d, 2d, 3.5d), 1d)
    testDoubleAssertion(Array(1d, 2d, 3d), Array(1d, 2d, 3.5d), 0.1d, NotEquals)
    testDoubleAssertion(Array(1d, 2d, 3d), Array(1d, 2d, 3.5d), 0.1d, NotEquals)
  }

  @Test
  def testAssertArrayEqualsFloats(): Unit = {
    def testFloatAssertion(expected: Array[Float], actual: Array[Float],
        delta: Float, equals: Boolean = true): Unit = {
      val message = "Should be different up to != operator"
      testIfAsserts(assertArrayEquals(message, expected, actual, delta), equals)
      testIfAsserts(assertArrayEquals(expected, actual, delta), equals)
    }

    testFloatAssertion(Array(1f, 2f, 3f), Array(1f, 2f, 4f), 1f)
    testFloatAssertion(Array(1f, 2f, 3f), Array(1f, 2f, 3.5f), 1f)
    testFloatAssertion(Array(1f, 2f, 3f), Array(1f, 2f, 4f), 0.11f, NotEquals)
    testFloatAssertion(Array(1f, 2f, 3f), Array(1f, 2f, 3.5f), 0.11f, NotEquals)
  }

  @Test
  def testAssertThat(): Unit = {
    testIfAsserts(assertThat("42", instanceOf[String](classOf[String])))
    testIfAsserts(assertThat("42", instanceOf[String](classOf[Int])), ShallNotPass)

    testIfAsserts(assertThat(42, instanceOf(classOf[Int])))
    testIfAsserts(assertThat(42, instanceOf[Int](classOf[Long])), ShallNotPass)
    testIfAsserts(assertThat(42, instanceOf[Int](classOf[String])), ShallNotPass)

    testIfAsserts(assertThat(Float.MaxValue, instanceOf(classOf[Float])))
    testIfAsserts(assertThat(Double.MaxValue, instanceOf(classOf[Double])))
  }

  @Test
  def testExpectThrows(): Unit = {
    testIfAsserts(expectThrows(classOf[Exception], throw new Exception))
    testIfAsserts(expectThrows(classOf[IndexOutOfBoundsException],
        throw new IndexOutOfBoundsException))

    testIfAsserts {
      val ex = expectThrows(classOf[Exception], throw new Exception("abc"))
      assertEquals(ex.getMessage, "abc")
    }

    testIfAsserts(expectThrows(classOf[IndexOutOfBoundsException],
        throw new Exception), ShallNotPass)
    testIfAsserts(expectThrows(classOf[IndexOutOfBoundsException],()),
        ShallNotPass)
  }

  @Test
  def testAssertThrows(): Unit = {
    testIfAsserts(assertThrows(classOf[Exception], throw new Exception))
    testIfAsserts(assertThrows(classOf[IndexOutOfBoundsException],
        throw new IndexOutOfBoundsException))

    testIfAsserts(assertThrows(classOf[IndexOutOfBoundsException],
        throw new Exception), ShallNotPass)
    testIfAsserts(assertThrows(classOf[IndexOutOfBoundsException], ()),
        ShallNotPass)
  }

  @Test def testIfAssertsTest_issue_2252(): Unit = {
    Try(testIfAsserts(())) match {
      case Success(_) => // As expected

      case Failure(ex) =>
        val msg = "testIfAsserts should not have thrown."
        throw new AssertionError(msg).initCause(ex)
    }

    Try(testIfAsserts((), ShallNotPass)) match {
      case Success(_) =>
        fail("testIfAsserts should have failed")

      case Failure(ex: AssertionError)
          if ex.getMessage == "Assertion should have failed." =>
        // As expected

      case Failure(ex) =>
        throw ex
    }

    Try(testIfAsserts(throw new AssertionError)) match {
      case Success(_) =>
        fail("testIfAsserts should not succeed with <throw new AssertionError>")

      case Failure(ex: AssertionError)
          if ex.getMessage == "Assertion should not have failed." =>
        // As expected

      case Failure(ex) => throw ex
    }

    Try(testIfAsserts(throw new AssertionError, ShallNotPass)) match {
      case Success(_) =>
        // As expected

      case Failure(ex: AssertionError)
          if ex.getMessage == "Assertion should have failed." =>
        fail("testIfAsserts should have succeed with <throw new AssertionError>")

      case Failure(ex) => throw ex
    }

    val except = new Exception
    Try(testIfAsserts(throw except)) match {
      case Success(_) =>
        fail("testIfAsserts should not succeed with <throw new Exception>")

      case Failure(ex) =>
        if (ex ne except)
          throw ex
    }

    Try(testIfAsserts(throw except, ShallNotPass)) match {
      case Success(_) =>
        fail("testIfAsserts should not succeed with <throw new Exception>")

      case Failure(ex) =>
        if (ex ne except)
          throw ex
    }
  }
}
