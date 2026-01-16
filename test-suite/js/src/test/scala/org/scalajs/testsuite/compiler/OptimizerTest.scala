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
import scala.scalajs.js.annotation._

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class OptimizerTest {
  import OptimizerTest._

  // Inlineable classes

  @Test def updateFieldsOfThisInTheComputationOfOtherFields_Issue1153(): Unit = {
    val foo = new InlineClassDependentFields(5)
    assertEquals(5, foo.x)
    assertTrue(foo.b)
    assertEquals(11, foo.y)
  }

  @Test def assignThisToField(): Unit = {
    val foo = new InlineClassThisAlias(5)
    assertEquals(5, foo.z)
  }

  // Optimizer regression tests

  @Test def timesNegativeOneForInt_Issue1453(): Unit = {
    @noinline
    def start0: Int = (() => 10)()

    val start = start0
    val step = -1
    val numRangeElements = start - 1
    val lastElement = start + (numRangeElements - 1) * step
    assertEquals(2, lastElement)
  }

  @Test def timesNegativeOneForFloatAndDouble_Issue1478(): Unit = {
    @noinline
    def a: Float = (() => 5.0f)()
    assertEquals(-5.0f, a * -1.0f, 0.0)

    @noinline
    def b: Double = (() => 7.0)()
    assertEquals(-7.0, b * -1.0, 0.0)
  }

  @Test def foreachOnDownwardRange_Issue1453(): Unit = {
    @noinline
    def start0: Int = (() => 10)()

    val elements = js.Array[Int]()
    for (i <- start0 to 2 by -1) {
      if (i < 0)
        throw new AssertionError("Going into infinite loop")
      elements.push(i)
    }
    assertArrayEquals(Array(10, 9, 8, 7, 6, 5, 4, 3, 2), elements.toArray)
  }

  @Test def classOfTEqEqClassOfU_Issue1658(): Unit = {
    assertEquals(classOf[String], classOf[String])
    assertEquals(classOf[Int], classOf[Int])
    assertEquals(classOf[Array[Int]], classOf[Array[Int]])
    assertEquals(classOf[Array[String]], classOf[Array[String]])

    assertFalse(classOf[String] == classOf[Int])
    assertFalse(classOf[Seq[_]] == classOf[List[_]])
    assertFalse(classOf[Array[Int]] == classOf[Array[Integer]])
    assertFalse(classOf[Array[Object]] == classOf[Array[Integer]])
    assertFalse(classOf[String] == classOf[Array[String]])
    assertFalse(classOf[Array[Array[Object]]] == classOf[Array[Object]])
  }

  @Test def sideEffectDiscardInEliminatedBinding_Issue2467(): Unit = {
    val b = Array.newBuilder[AnyRef]
    def mockPrintln(x: Any): Unit =
      b += ("" + x)

    def get[T](x: T) = { mockPrintln("get: " + x); x }

    def bn2(a: Int, b: => Int)(c: Int = b) = a + b
    mockPrintln(bn2(b = get(2), a = get(1))()) // should get: 1, 2, 2

    assertArrayEquals(Array[AnyRef]("get: 1", "get: 2", "get: 2", "3"),
        b.result())
  }

  @Test def testBitsetOrEq_Issue2523(): Unit = {
    import scala.collection.mutable.BitSet

    val b0 = BitSet(5, 6)
    val b1 = BitSet(7)
    val b2 = BitSet(1, 5)
    val b3 = BitSet(6, 7)
    val b4 = BitSet(6, 7)

    b1 |= b0
    assertEquals("BitSet(5, 6, 7)", b1.toString)
    b2 &= b0
    assertEquals("BitSet(5)", b2.toString)
    b3 ^= b0
    assertEquals("BitSet(5, 7)", b3.toString)
    b4 &~= b0
    assertEquals("BitSet(7)", b4.toString)
    b0 ^= b0 |= b1
    assertEquals("BitSet(5, 6, 7)", b0.toString)
  }

  @Test def keepBreakToLabelWithinFinallyBlock_Issue2689(): Unit = {
    // scalastyle:off return
    val logs = js.Array[String]()

    @noinline def log(s: String): Unit =
      logs += s

    def a1(): Unit = log("a1")

    def i1: Int = { log("i1"); 1 }
    def i2: Int = { log("i2"); 2 }

    def e1: Int = { log("e1"); throw new Exception("Boom! #2689") }

    def t4(i: => Int): Int = {
      log("t4")
      try {
        return i
      } finally {
        return i2
      }
    }

    assertEquals(2, t4(i1))
    assertArrayEquals(Array[AnyRef]("t4", "i1", "i2"), logs.toArray[AnyRef])
    logs.clear()

    assertEquals(2, t4(e1))
    assertArrayEquals(Array[AnyRef]("t4", "e1", "i2"), logs.toArray[AnyRef])
    logs.clear()
    // scalastyle:on return
  }

  @Test def preserveSideEffectsInWrapAsThrowable(): Unit = {
    var i: Int = 1
    val x =
      if (i > 0) js.special.wrapAsThrowable({ i += 1; i })
      else 42

    x match {
      case js.JavaScriptException(y) =>
        assertEquals(2, y)
    }
    assertEquals(2, i)
  }

  @Test def preserveSideEffectsInUnwrapFromThrowable(): Unit = {
    var i: Int = 1
    val x =
      if (i > 0) js.special.unwrapFromThrowable({ i += 1; new js.JavaScriptException(i) })
      else 42
    assertEquals(2, x)
    assertEquals(2, i)
  }

  @Test def preserveSideEffectsInUnwrapFromThrowableInThrow(): Unit = {
    var i: Int = 1
    try {
      if (i > 0)
        throw ({ i += 1; new js.JavaScriptException(i) })
      i = -1 // unreachable
    } catch {
      case js.JavaScriptException(x) =>
        assertEquals(2, x)
        assertEquals(2, i)
    }
    assertEquals(2, i)
  }

  // === constant folding

  @Test def constantFoldingEqEqEq(): Unit = {
    @inline def test(expectEq: Boolean, lhs: Any, rhs: Any): Unit = {
      assertEquals(expectEq,
          lhs.asInstanceOf[AnyRef] eq rhs.asInstanceOf[AnyRef])
      assertEquals(!expectEq,
          lhs.asInstanceOf[AnyRef] ne rhs.asInstanceOf[AnyRef])
    }

    test(true, false, false)
    test(true, 5, 5)
    test(true, 5.toByte, 5.toByte)
    test(true, 5.toByte, 5)
    test(true, 5.0, 5)
    test(true, 5.0f, 5.toShort)
    test(true, classOf[String], classOf[String])
    test(true, "hello", "hello")

    test(false, false, true)
    test(false, 'A', 'A') // they're boxed, so not ===
    test(false, 5, 6)
    test(false, 5.toByte, 6.toByte)
    test(false, 5.toByte, 5L)
    test(false, 5, 5L)
    test(false, 5L, 6L)
    test(false, false, 0)
    test(false, 65, 'A')
    test(false, classOf[String], classOf[Boolean])
    test(false, "hello", "world")

    /* When using BigInts for Longs, equal Longs will be ===, but not when
     * using RuntimeLongs since the instances will be different.
     */
    val usingBigIntForLongs = js.typeOf(5L) == "bigint"
    test(usingBigIntForLongs, 5L, 5L)
  }

  @Test def constantFoldingEqEq(): Unit = {
    @inline def testChar(expectEq: Boolean, lhs: Char, rhs: Char): Unit = {
      assertEquals(expectEq, lhs == rhs)
      assertEquals(!expectEq, lhs != rhs)
    }

    testChar(true, 'A', 'A')
    testChar(false, 'A', 'B')

    @inline def testInt(expectEq: Boolean, lhs: Int, rhs: Int): Unit = {
      assertEquals(expectEq, lhs == rhs)
      assertEquals(!expectEq, lhs != rhs)
    }

    testInt(true, 5, 5)
    testInt(false, 5, 6)

    @inline def testLong(expectEq: Boolean, lhs: Long, rhs: Long): Unit = {
      assertEquals(expectEq, lhs == rhs)
      assertEquals(!expectEq, lhs != rhs)
    }

    testLong(true, 5L, 5L)
    testLong(false, 5L, 6L)

    @inline def testDouble(expectEq: Boolean, lhs: Double, rhs: Double): Unit = {
      assertEquals(expectEq, lhs == rhs)
      assertEquals(!expectEq, lhs != rhs)
    }

    testDouble(true, 5.5, 5.5)
    testDouble(false, 5.5, 6.5)
  }

  // +[string] constant folding

  @Test def foldingTwoConstantStrings(): Unit = {
    @inline def str: String = "I am "
    assertEquals("I am constant", str + "constant")
  }

  @Test def foldingTheEmptyStringWithString(): Unit = {
    @noinline def str: String = "hello"
    assertEquals("hello", str + "")
    assertEquals("hello", "" + str)
  }

  @Test def folding1Point4fAndString(): Unit = {
    assertEquals("1.399999976158142hello", 1.4f + "hello")
    assertEquals("hello1.399999976158142", "hello" + 1.4f)
  }

  @Test def foldingCascadingPlusString(): Unit = {
    @noinline def str: String = "awesome! 10/10"
    assertEquals("Scala.js is awesome! 10/10", "Scala.js" + (" is " + str))
    assertEquals("awesome! 10/10 is Scala.js", (str + " is ") + "Scala.js")
  }

  @Test def foldingChainOfPlusString(): Unit = {
    @inline def b: String = "b"
    @inline def d: String = "d"
    @inline def f: String = "f"
    assertEquals("abcdefg", "a" + b + "c" + d + "e" + f + "g")
  }

  @Test def foldingDouble1Point0AndString(): Unit = {
    assertEquals("1hello", 1.0 + "hello")
    assertEquals("hello1", "hello" + 1.0)
  }

  @Test def foldingZeroAndString(): Unit = {
    assertEquals("0hello", 0.0 + "hello")
    assertEquals("hello0", "hello" + 0.0)
    assertEquals("0hello", -0.0 + "hello")
    assertEquals("hello0", "hello" + (-0.0))
  }

  @Test def foldingInfinitiesAndString(): Unit = {
    assertEquals("Infinityhello", Double.PositiveInfinity + "hello")
    assertEquals("helloInfinity", "hello" + Double.PositiveInfinity)
    assertEquals("-Infinityhello", Double.NegativeInfinity + "hello")
    assertEquals("hello-Infinity", "hello" + Double.NegativeInfinity)
  }

  @Test def foldingNaNAndString(): Unit = {
    assertEquals("NaNhello", Double.NaN + "hello")
    assertEquals("helloNaN", "hello" + Double.NaN)
  }

  @Test def foldingDoubleWithDecimalAndString(): Unit = {
    assertEquals("1.2323919403474454e+21hello", 1.2323919403474454e21 + "hello")
    assertEquals("hello1.2323919403474454e+21", "hello" + 1.2323919403474454e21)
  }

  @Test def foldingDoubleThatJVMWouldPrintInScientificNotationAndString(): Unit = {
    assertEquals("123456789012345hello", 123456789012345d + "hello")
    assertEquals("hello123456789012345", "hello" + 123456789012345d)
  }

  @Test def foldingDoublesToString(): Unit = {
    @noinline def toStringNoInline(v: Double): String = v.toString
    @inline def test(v: Double): Unit =
      assertEquals(toStringNoInline(v), v.toString)

    // Special cases
    test(0.0)
    test(-0.0)
    test(Double.NaN)
    test(Double.PositiveInfinity)
    test(Double.NegativeInfinity)

    // k <= n <= 21
    test(1.0)
    test(12.0)
    test(123.0)
    test(1234.0)
    test(12345.0)
    test(123456.0)
    test(1234567.0)
    test(12345678.0)
    test(123456789.0)
    test(1234567890.0)
    test(12345678901.0)
    test(123456789012.0)
    test(1234567890123.0)
    test(12345678901234.0)
    test(123456789012345.0)
    test(1234567890123456.0)
    test(12345678901234657.0)
    test(123456789012345678.0)
    test(1234567890123456789.0)
    test(12345678901234567890.0)
    test(123456789012345678901.0)

    // 0 < n <= 21
    test(1.42)
    test(12.42)
    test(123.42)
    test(1234.42)
    test(12345.42)
    test(123456.42)
    test(1234567.42)
    test(12345678.42)
    test(123456789.42)
    test(1234567890.42)
    test(12345678901.42)
    test(123456789012.42)
    test(1234567890123.42)
    test(12345678901234.42)
    test(123456789012345.42)
    test(1234567890123456.42)
    test(12345678901234657.42)
    test(123456789012345678.42)
    test(1234567890123456789.42)
    test(12345678901234567890.42)
    test(123456789012345678901.42)

    // -6 < n <= 0
    test(0.1)
    test(0.01)
    test(0.001)
    test(0.0001)
    test(0.00001)
    test(0.000001)

    // k == 1
    test(1e22)
    test(2e25)
    test(3e50)
    test(4e100)
    test(5e200)
    test(6e300)
    test(7e307)
    test(1e-22)
    test(2e-25)
    test(3e-50)
    test(4e-100)
    test(5e-200)
    test(6e-300)
    test(7e-307)

    // else
    test(1.42e22)
    test(2.42e25)
    test(3.42e50)
    test(4.42e100)
    test(5.42e200)
    test(6.42e300)
    test(7.42e307)
    test(1.42e-22)
    test(2.42e-25)
    test(3.42e-50)
    test(4.42e-100)
    test(5.42e-200)
    test(6.42e-300)
    test(7.42e-307)

    // special cases when ulp > 1
    test(18271179521433728.0)
    test(1.15292150460684685e18)
    test(1234567890123456770.0)
    test(2234567890123456770.0)
    test(4234567890123450000.0)
    test(149170297077708820000.0)
    test(296938164846899230000.0)
    test(607681513323520000000.0)
  }

  @Test def foldingLongAndString(): Unit = {
    assertEquals("1hello", 1L + "hello")
    assertEquals("hello1", "hello" + 1L)
  }

  @Test def foldingIntegerAndString(): Unit = {
    assertEquals("42hello", 42 + "hello")
    assertEquals("hello42", "hello" + 42)
  }

  @Test def foldingBooleanAndString(): Unit = {
    assertEquals("false is not true", "false is not " + true)
  }

  @Test def foldingUnitAndString(): Unit = {
    assertEquals("undefined is undefined", "undefined is " + ())
  }

  @Test def foldingNullAndString(): Unit = {
    assertEquals("Damien is not null", "Damien is not " + null)
  }

  @Test def foldingCharAndString(): Unit = {
    assertEquals("Scala.js", 'S' + "cala.js")
    assertEquals("Scala.js", "Scala.j" + 's')
  }

  // Division by zero

  @Test def divideByZero_Issue4604(): Unit = {
    // Ints

    @noinline def intDivByZeroInExpressionPosition(): Int = {
      0 / 0
    }

    @noinline def intDivByZeroInStatementPosition(): Unit = {
      0 / 0
      fail("should be unreachable")
    }

    assertThrows(classOf[ArithmeticException], intDivByZeroInExpressionPosition())
    assertThrows(classOf[ArithmeticException], intDivByZeroInStatementPosition())

    // Longs

    @noinline def longDivByZeroInExpressionPosition(): Long = {
      0L / 0L
    }

    @noinline def longDivByZeroInStatementPosition(): Unit = {
      0L / 0L
      fail("should be unreachable")
    }

    assertThrows(classOf[ArithmeticException], longDivByZeroInExpressionPosition())
    assertThrows(classOf[ArithmeticException], longDivByZeroInStatementPosition())
  }

  // Virtualization of JSArrayConstr

  @Test def virtualizedJSArrayConstr(): Unit = {
    @noinline def b = 42

    val a = js.Array[Any]("hello", b)

    assertEquals(2, a.length)
    assertEquals("hello", a(0))
    assertEquals(42, a(1))
    assertEquals(js.undefined, a(-1))
    assertEquals(js.undefined, a(2))
  }

  @Test def escapedJSArrayConstr(): Unit = {
    @noinline def escape[A](a: A): A = a

    val a = js.Array[Any]("hello", 42)

    assertEquals(2, a.length)
    assertEquals("hello", a(0))
    assertEquals(42, a(1))
    assertEquals(js.undefined, a(-1))
    assertEquals(js.undefined, a(2))

    assertEquals(2, escape(a).length)
  }

  @Test def modifiedJSArrayConstr(): Unit = {
    @noinline def escape[A](a: A): A = a

    val a = js.Array[Any]("hello", 42)

    assertEquals(2, a.length)
    assertEquals("hello", a(0))
    assertEquals(42, a(1))
    assertEquals(js.undefined, a(-1))
    assertEquals(js.undefined, a(2))

    a(0) = "bar"

    assertEquals("bar", a(0))
  }

  @Test def virtualizedJSArrayConstrInSpread(): Unit = {
    class Foo extends js.Object {
      def check(a: Int, b: String, rest: Any*): Unit = {
        assertEquals(5, a)
        assertEquals("foobar", b)
        assertEquals(2, rest.length)
        assertEquals("hello", rest(0))
        assertEquals(42, rest(1))
      }
    }

    val a = js.Array[Any]("hello", 42)
    val foo = new Foo
    foo.check(5, "foobar", a.toIndexedSeq: _*)
  }

  @Test def virtualizedTuple(): Unit = {
    @noinline def b = 42

    val a = js.Tuple2("hello", b)

    assertEquals("hello", a._1)
    assertEquals(42, a._2)
  }

  @Test def escapedTuple(): Unit = {
    @noinline def escape[A](a: A): A = a

    val a = js.Tuple2("hello", 42)

    assertEquals("hello", a._1)
    assertEquals(42, a._2)

    assertEquals("hello", escape(a)._1)
  }

  // Bug #3415

  @Test def infiniteRecursionInlining_Issue3415(): Unit = {
    assumeTrue("linking only", false)
    doWhile1("foo")(f => f(true))
  }

  @inline def doWhile1[Domain2](endDoWhile1: => Domain2)(
      condition2: (Boolean => Domain2) => Domain2): Domain2 = {
    condition2 { (conditionValue2) =>
      if (conditionValue2)
        doWhile1[Domain2](endDoWhile1)(condition2)
      else
        endDoWhile1
    }
  }

  @Test def infiniteRecursionInliningPlaceholder_Issue3415(): Unit = {
    assumeTrue("linking only", false)
    doWhile(???)
  }

  @inline def doWhile(
      condition: js.Function1[js.Function1[Boolean, String], String]): String = {
    condition { (conditionValue: Boolean) =>
      doWhile(condition)
    }
  }

  @Test def keepQualifierSideEffectsOfEliminatedField(): Unit = {
    @noinline
    class Foo(var x: Int)

    val foo = new Foo(2)

    var called = false

    @noinline
    def getFoo() = {
      called = true
      foo
    }

    getFoo().x = 1

    assertTrue(called)
  }

  @Test def keepQualifierSideEffectsOfEliminatedFieldInline(): Unit = {
    @inline
    class Foo(var x: Int)

    val foo = new Foo(2)

    var called = false

    @inline
    def getFoo() = {
      called = true
      foo
    }

    getFoo().x = 1

    assertTrue(called)
  }

  @Test def keepQualifierSideEffectsOfEliminatedJSField(): Unit = {
    class Foo extends js.Object {
      private[this] var x: Int = 1

      @inline
      final private[OptimizerTest] def set() = {
        x = 2
      }
    }

    val foo = new Foo
    var called = false

    def getFoo() = {
      called = true
      foo
    }

    getFoo().set()

    assertTrue(called)
  }

  @Test def expandedRTAssertionOriginal_Issue5231(): Unit = {
    def expandedRTLongBug: (Array[Int], Long) => Long = { (array, z) =>
      array.foldRight(z)(_ - _)
    }

    assertEquals(-3L, expandedRTLongBug(Array(1, 2, 3), 5L))
  }

  @Test def expandedRTAssertionMinimal_Issue5231(): Unit = {
    @noinline def hideLong(x: Long): Long = x
    @noinline def hide(x: Any): Any = x

    val z0: Long = hideLong(5L)
    var i = 0
    var z: Any = z0
    while (i < 2) {
      z = hide(i.toLong)
      i += 1
    }
    assertEquals(1L, z)
  }

  @noinline // just to be sure
  @Test def inlineClassLabeledReturnDiscardStatementPos_Issue5246(): Unit = {
    @inline def makeBug(x: Int): Bug5246 = {
      // Use an explicit `return` to cause a Labeled block and its Return node
      return new Bug5246(x) // scalastyle:ignore
    }

    // Assign to val to cause a pretransform
    val bug = makeBug(5)
    // but then discard it and leave it in statement position
    val _ = bug
  }
}

object OptimizerTest {

  @inline
  class InlineClassDependentFields(val x: Int) {
    val b = x > 3
    val y = if (b) x + 6 else x - 2
  }

  @inline
  class InlineClassThisAlias(val x: Int) {
    val t = this
    val y = x
    val z = t.y
  }

  @inline
  final class Bug5246(val x: Int)

}
