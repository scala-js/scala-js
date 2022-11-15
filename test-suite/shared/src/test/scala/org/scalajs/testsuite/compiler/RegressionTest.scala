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

import scala.annotation.{switch, tailrec}

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

import org.scalajs.testsuite.utils.Platform
import org.scalajs.testsuite.utils.Platform._

class RegressionTest {
  import RegressionTest._

  @Test def wrongDivisionConversion7Divide2Pt0_Issue18(): Unit = {
    val div = 7 / 2.0
    assertEquals(3.5, div, 0.0)
    assertEquals("double", div.getClass.getName)

    val mod = 7 % 2.0
    assertEquals(1.0, mod, 0.0)
    assertEquals("double", mod.getClass.getName)
  }

  @Test def abortWithSomePatternMatchGuards_Issue22(): Unit = {
    object PatternMatchGuards {
      def go(f: Int => Int): Int = f(1)
      def main(): Unit = {
        go {
          case x if false => x
        }
      }
    }
    // Nothing to check
  }

  @Test def badEncodingForCharactersSpanning2UTF16Chars_Issue23(): Unit = {
    val str = "A∀\uD835\uDCAB"
    var s: String = ""
    for (c <- str) {
      val code: Int = c
      s = s + code + " "
    }
    assertEquals("65 8704 55349 56491 ", s)
  }

  @Test def characterEscapes_Issue3125(): Unit = {
    val str = {
      // The space at the end is intended. It is 0x20.
      "\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0008\u0009\u000a" +
      "\u000b\u000c\u000d\u000e\u000f\u0010\u0011\u0012\u0013\u0014\u0015" +
      "\u0016\u0017\u0018\u0019\u001a\u001b\u001c\u001d\u001e\u001f "
    }

    for (i <- 0 until str.length)
      assertEquals(i, str.charAt(i).toInt)

    val strQuotes = "\"'"
    assertEquals(34, strQuotes.charAt(0).toInt)
    assertEquals(39, strQuotes.charAt(1).toInt)
  }

  @Test def emitStaticCallsWhenForwardingToAnotherConstructor_Issue66(): Unit = {
    new Bug66B("", "")
  }

  @Test def callSubSequenceOnNonStringCharSequences_Issue55(): Unit = {
    val arr: CharSequence = java.nio.CharBuffer.wrap(Array('a', 'b', 'c', 'd'))
    val ss = arr.subSequence(2, 3)
    assertEquals(1, ss.length())
    assertEquals('c', ss.charAt(0))
  }

  @Test def concatPrimitiveValuesToStrings_Issue113(): Unit = {
    assertEquals("4foo", 4 + "foo")
    assertEquals("afoo", 'a' + "foo")
  }

  @Test def dispatchCallsOnPrivateFunctions_Issue165(): Unit = {
    class A {
      private def x: Int = 1
      def value: Int = x
    }
    class B extends A {
      private def x: Int = 2
    }
    assertEquals(1, new B().value)
  }

  @Test def classLiteralsForExistentialValueTypes_Issue218(): Unit = {
    import Platform.scalaVersion

    assumeFalse("Affected by https://github.com/scala/bug/issues/10551",
        Platform.executingInJVM && {
          scalaVersion.startsWith("2.11.") ||
          scalaVersion == "2.12.0" || scalaVersion == "2.12.1" ||
          scalaVersion == "2.12.2" || scalaVersion == "2.12.3" ||
          scalaVersion == "2.12.4"
        })

    assertEquals("org.scalajs.testsuite.compiler.RegressionTest$Bug218Foo",
        scala.reflect.classTag[Bug218Foo[_]].toString)
  }

  @Test def buffer_Issue268(): Unit = {
    val a = scala.collection.mutable.Buffer.empty[Int]
    a.insert(0, 0)
    a.remove(0)
    for (i <- 0 to 10) {
      a.insert(a.length / 2, i)
    }
    assertEquals("1, 3, 5, 7, 9, 10, 8, 6, 4, 2, 0", a.mkString(", "))
  }

  @Test def doNotCallEqualsWhenComparingWithLiteralNull_Issue362(): Unit = {
    // scalastyle:off equals.hash.code
    class A {
      override def equals(x: Any): Boolean = !(this == null)
    }
    // scalastyle:on equals.hash.code

    val x = new A
    val y = new A

    // If the null comparisons actually call equals, the following two will
    // cause infinite recursion
    assertEquals(y, x)
    assertEquals(x, y)
  }

  @Test def unboxNullToTheZeroOfTypes_Issue674(): Unit = {
    class Box[A] {
      var value: A = _
    }
    def zero[A]: A = new Box[A].value

    /* Note: the same shape of test for Unit does not work, but it seems to
     * be a problem in scalac because it does not work on the JVM either.
     */

    val bool = zero[Boolean]
    assertTrue((bool: Any).isInstanceOf[Boolean])
    assertEquals(false, bool) // scalastyle:ignore

    val char = zero[Char]
    assertTrue((char: Any).isInstanceOf[Char])
    assertEquals('\u0000', char)

    val byte = zero[Byte]
    assertTrue((byte: Any).isInstanceOf[Byte])
    assertEquals(0.toByte, byte)

    val short = zero[Short]
    assertTrue((short: Any).isInstanceOf[Short])
    assertEquals(0.toShort, short)

    val int = zero[Int]
    assertTrue((int: Any).isInstanceOf[Int])
    assertEquals(0, int)

    val long = zero[Long]
    assertTrue((long: Any).isInstanceOf[Long])
    assertEquals(0L, long)

    val float = zero[Float]
    assertTrue((float: Any).isInstanceOf[Float])
    assertEquals(0.0f, float, 0.0f)

    val double = zero[Double]
    assertTrue((double: Any).isInstanceOf[Double])
    assertEquals(0.0, double, 0.0)

    val ref = zero[AnyRef]
    assertEquals(null, ref)
  }

  @Test def paramDefsInTailrecMethodsAreMutable_Issue825(): Unit = {
    @tailrec
    def foo(x: Int, y: Int): Unit = {
      if (x < y) foo(y, x)
      else {
        assertEquals(4, x)
        assertEquals(2, y)
      }
    }
    foo(2, 4)
  }

  @Test def nullSynchronizedThrows_Issue874(): Unit = {
    assertThrows(classOf[NullPointerException], null.synchronized(5))
  }

  @Test def synchronizedXPreservesSideEffectsOfX(): Unit = {
    var c = 0
    def x: RegressionTest.this.type = { c += 1; this }
    assertEquals(5, x.synchronized(5))
    assertEquals(1, c)
  }

  @Test def irCheckerAllowsApplySelectOnNullType_Issue1123(): Unit = {
    /* The IR checker checks this code whether or not the assumption holds.
     * The assumption only applies to the run-time behavior.
     */
    assumeTrue("assuming compliant null pointer checks", hasCompliantNullPointers)

    def giveMeANull(): Null = null
    assertThrows(classOf[NullPointerException], (giveMeANull(): StringBuilder).append(5))
    assertThrows(classOf[NullPointerException], (giveMeANull(): scala.runtime.IntRef).elem)
  }

  @Test def irCheckerAllowsApplySelectOnNothingType_Issue1123(): Unit = {
    def giveMeANothing(): Nothing = throw new IllegalStateException("boom")
    assertThrows(classOf[IllegalStateException], (giveMeANothing(): StringBuilder).append(5))
    assertThrows(classOf[IllegalStateException], (giveMeANothing(): scala.runtime.IntRef).elem)
  }

  @Test def irCheckerDoesNotCheckFieldExistenceOnNonExistentClasses(): Unit = {
    // In this test, Outer is not "needed at all"

    class Outer(x: Int) {
      class Inner {
        def get(): Int = x
      }
    }

    def test(outer: Outer): Int = {
      if (outer == null) {
        3
      } else {
        val inner = new outer.Inner
        inner.get()
      }
    }

    assertEquals(3, test(null))
  }

  @Test def irCheckerDoesNotCheckFieldExistenceOnClassesWithNoInstance_Issue3060(): Unit = {
    // In this test, Outer is "needed at all", but does not have any instance

    class Outer(x: Int) {
      class Inner {
        def get(): Int = x
      }
    }

    def test(outer: Outer): Int = {
      if (outer == null) {
        3
      } else {
        val inner = new outer.Inner
        inner.get()
      }
    }

    // make sure Outer is "needed at all"
    assertFalse(classOf[Outer].isInterface)

    assertEquals(3, test(null))
  }

  @Test def irCheckerDoesNotCheckMethodSignaturesOnClassesWithNoInstance(): Unit = {
    assumeTrue("linking only", false)

    class Foo // this class will be dropped by base linking

    class Bar {
      /* This method is called, but unreachable because there are no instances
       * of `Bar`. It will therefore not make `Foo` reachable.
       */
      def meth(foo: Foo): String = foo.toString()
    }

    @noinline def nullBar(): Bar = null

    // the IR checker must not try to infer the signature of these calls
    nullBar().meth(null)
    (null: Bar).meth(null)
    (??? : Bar).meth(null) // scalastyle:ignore
  }

  @Test def orderCtorStatementsWhenInlining_Issue1369(): Unit = {
    trait Bar {
      def x: Int
      var y = x + 1
    }

    @inline
    class A(var x: Int) extends Bar

    val obj = new A(1)
    assertEquals(1, obj.x)
    assertEquals(2, obj.y)
  }

  @Test def doNotRestrictMutabilityOfFields_Issue1021(): Unit = {
    class A {
      /* This var is referred to in the lambda passed to `foreach`. Therefore
       * it is altered in another compilation unit (even though it is
       * private[this]).
       * This test makes sure the compiler doesn't wrongly mark it as
       * immutable because it is not changed in its compilation unit itself.
       */
      private[this] var x: Int = 1

      def get: Int = x

      def foo(): Unit =
        Seq(2).foreach(x = _)
    }

    val a = new A()
    assertEquals(1, a.get)
    a.foo()
    assertEquals(2, a.get)
  }

  @Test def populateDesugarEnvironmentsWithClosureParams_Issue1399(): Unit = {
    /* To query whether a field is mutable, the JSDesugar needs to first
     * unnest a statement block from an argument list, and then unnest the
     * parameter under test.
     * It will then test, if it is immutable, which will trigger an
     * environment lookup.
     */

    // We need a true class for @noinline to work
    class Test {
      @noinline
      def concat(x: Any, y: Any): String = x.toString + y.toString

      @noinline
      def fct: Function1[Any, String] = { (v: Any) => // parameter under test
        /* Pass `v` as a first parameter, a true block as a second parameter.
         * Note that this only works after optimizations, because `v` is first
         * asInstanceOfd to Object and hence not the original `v` is used in
         * the call itself.
         * The optimizer eliminates the useless asInstanceOf.
         */
        concat(v, {
          // This must be a true block
          var x = 1
          while (x < 5) x += 1
          x
        })
      }
    }

    assertEquals("15", new Test().fct(1))
  }

  @Test def doNotCauseClosureToCrashWithUnexpectedVariableNaN_Issue1469(): Unit = {
    /* Basically we want to make sure that a specialized bridge of Function1
     * taking and returning Double is emitted (and not dce'ed) for this
     * class F, which actually returns Unit.
     * This, after optimizations, causes something like
     *   +(apply__V(x), (void 0))
     * to be emitted (inlining the bridge returning Any into the bridge
     * returning Double).
     * This in turn causes Closure to constant fold +(void 0) into NaN,
     * which used to trigger the
     *   Internal Compiler Error: Unexpected variable NaN
     * Note that we *cannot* actually call that bridge on F, because we would
     * run into undefined behavior! So we have another function that actually
     * returns a Double, and we use to make sure that
     * Function1.apply(Double)Double is reachable, which will make it
     * reachable also for F.
     */
    class F extends Function1[Any, Unit] {
      def apply(x: Any): Unit =
        assertEquals(5, x)
    }

    // Make sure the specialized Function1.apply(Double)Double is reachable.
    @noinline def makeFun(y: Double): Double => Double = {
      val z = y + 1.5
      ((x: Double) => x * z): (Double => Double)
    }
    val someDoubleFun = makeFun(2.0)
    assertEquals(147.0, someDoubleFun(42.0), 0.0)

    // Make sure F itself is reachable and not completely inlineable
    @noinline def makeF: Any => Any = (() => new F)()
    val f = makeF
    f(5)
  }

  @Test def switchMatchWith2GuardsForTheSameValue_Issue1589(): Unit = {
    @noinline def genB(): Int = 0xE1
    val b = genB()
    val x = b >> 4 match {
      case 0xE if b == 0xE0 =>
        4
      case 0xE if b == 0xE1 =>
        5
    }
    assertEquals(5, x)
  }

  @Test def switchMatchWithGuardAndResultTypeOfBoxedUnit_Issue1955(): Unit = {
    val bug = new Bug1955
    bug.bug(2, true)
    assertEquals(0, bug.result)
    bug.bug(1, true)
    assertEquals(579, bug.result)
    assertThrows(classOf[MatchError], bug.bug(2, false))
  }

  @Test def switchMatchWithGuardInStatementPosButWithNonUnitBranches_Issue4105(): Unit = {
    def encodeString(string: String, isKey: Boolean): String = {
      val buffer = new java.lang.StringBuilder()
      val length = string.length
      var index = 0
      while (index < length) {
        val ch = string.charAt(index)
        (ch: @switch) match { // note that this is a switch, in statement position
          case '\t' =>
            buffer.append("\\t") // note that all branches return a StringBuilder
          case '\n' =>
            buffer.append("\\n")
          case '\f' =>
            buffer.append("\\f")
          case '\r' =>
            buffer.append("\\r")
          case '\\' | '#' | '!' | '=' | ':' =>
            buffer.append('\\')
            buffer.append(ch)
          case ' ' if isKey => // note the guard here!
            buffer.append("\\ ")
          case _ =>
            buffer.append(ch)
        }
        index += 1
      }
      buffer.toString()
    }

    assertEquals("abc", encodeString("abc", false))
    assertEquals("abc", encodeString("abc", true))
    assertEquals("abc def", encodeString("abc def", false))
    assertEquals("abc\\ def", encodeString("abc def", true))
    assertEquals("1\\t2\\n3\\f4\\r5\\\\6\\!7 8a9", encodeString("1\t2\n3\f4\r5\\6!7 8a9", false))
    assertEquals("1\\t2\\n3\\f4\\r5\\\\6\\!7\\ 8a9", encodeString("1\t2\n3\f4\r5\\6!7 8a9", true))
  }

  @Test def returnXMatchInt_Issue2928(): Unit = {
    // scalastyle:off return

    def testNonUnit(x: Int): Boolean = {
      return x match {
        case 1 => true
        case _ => false
      }
    }

    var r: Option[Boolean] = None

    def testUnit(x: Int): Unit = {
      return x match {
        case 1 => r = Some(true)
        case _ => r = Some(false)
      }
    }

    assertEquals(true, testNonUnit(1))
    assertEquals(false, testNonUnit(2))

    testUnit(1)
    assertEquals(Some(true), r)
    r = None
    testUnit(2)
    assertEquals(Some(false), r)

    // scalastyle:on return
  }

  @Test def returnXMatchString_Issue2928(): Unit = {
    // scalastyle:off return

    def testNonUnit(x: String): Boolean = {
      return x match {
        case "True" => true
        case _      => false
      }
    }

    var r: Option[Boolean] = None

    def testUnit(x: String): Unit = {
      return x match {
        case "True" => r = Some(true)
        case _      => r = Some(false)
      }
    }

    assertEquals(true, testNonUnit("True"))
    assertEquals(false, testNonUnit("not true"))

    testUnit("True")
    assertEquals(Some(true), r)
    r = None
    testUnit("not true")
    assertEquals(Some(false), r)

    // scalastyle:on return
  }

  @Test def returnXMatchList_Issue2928(): Unit = {
    // scalastyle:off return

    def testNonUnit(x: List[String]): Boolean = {
      return x match {
        case "True" :: Nil => true
        case _             => false
      }
    }

    var r: Option[Boolean] = None

    def testUnit(x: List[String]): Unit = {
      return x match {
        case "True" :: Nil => r = Some(true)
        case _             => r = Some(false)
      }
    }

    assertEquals(true, testNonUnit("True" :: Nil))
    assertEquals(false, testNonUnit("not true" :: Nil))
    assertEquals(false, testNonUnit("True" :: "second" :: Nil))

    testUnit("True" :: Nil)
    assertEquals(Some(true), r)
    r = None
    testUnit("not true" :: Nil)
    assertEquals(Some(false), r)
    r = None
    testUnit("True" :: "second" :: Nil)
    assertEquals(Some(false), r)

    // scalastyle:on return
  }

  @Test def nullAsInstanceOfUnitSucceeds_Issue1691(): Unit = {
    /* Avoid scalac's special treatment of `<literal null>.asInstanceOf[X]`.
     * It does have the benefit to test our constant-folder of that pattern,
     * once getNull() is inlined; and of our run-time implementation, when the
     * optimizer is disabled.
     */
    def getNull(): Any = null
    val x = getNull().asInstanceOf[Unit]: Any

    if (Platform.scalaVersion.startsWith("2.11.")) {
      assertNull(x.asInstanceOf[AnyRef])
    } else {
      // As of Scala 2.12.0-M5, null.asInstanceOf[Unit] (correctly) returns ()
      assertEquals((), x)
    }
  }

  @Test def lambdaParameterWithDash_Issue1790(): Unit = {
    val f = (`a-b`: Int) => `a-b` + 1
    assertEquals(6, f(5))
  }

  @Test def nestedLabeledBlockSortCircuitReturns_Issue2307(): Unit = {
    class UnsafeCrud(i: Int) {
      def unsafeUpdate(l: List[Any], i: Int, f: Any => Any): (List[Any], Any) = {
        def loop(l: List[Any], i: Int, prefix: List[Any]): (List[Any], List[Any], Any) = {
          l match {
            case hd :: (tl: List[Any]) =>
              if (i == 0) (prefix, f(hd) :: tl, hd)
              else loop(tl, i - 1, hd :: prefix)
            case _ =>
              throw new Exception("...")
          }
        }

        val loopR = loop(l, i, Nil)
        val prefix = loopR._1
        val v = loopR._3
        (prefix, v)
      }

      def apply(l: List[Any], f: Any => Any): (List[Any], Any) =
        unsafeUpdate(l, i, f)
    }

    val r = 10 :: "foo" :: 'x' :: 42 :: Nil
    val result = new UnsafeCrud(0).apply(r, _ => "newStr")
    assertEquals((Nil, 10), result)
  }

  private val hasEqEqJLFloatDoubleBug: Boolean = {
    val v = Platform.scalaVersion
    v.startsWith("2.11.") || v == "2.12.1"
  }

  def assertTrueUnlessEqEqJLFloatDoubleBug(actual: Boolean): Unit = {
    if (hasEqEqJLFloatDoubleBug)
      assertFalse(actual)
    else
      assertTrue(actual)
  }

  @Test def eqEqJLDouble(): Unit = {
    // Taken from run/sd329.scala in scala/scala

    def d1: Double = 0.0
    def d2: Double = -0.0
    def d3: Double = Double.NaN
    def d4: Double = Double.NaN
    assertTrue(d1 == d2)
    assertTrue(d3 != d4)

    def d1B: java.lang.Double = d1
    def d2B: java.lang.Double = d2
    def d3B: java.lang.Double = d3
    def d4B: java.lang.Double = d4
    assertTrueUnlessEqEqJLFloatDoubleBug(d1B == d2B)
    assertTrue(d1 == d1B)
    assertTrue(d1B == d1)
    assertTrueUnlessEqEqJLFloatDoubleBug(d3B != d4B)
    assertTrue(d3 != d4B)
    assertTrue(d3B != d4)

    assertFalse(d1B.equals(d2B)) // ! see javadoc
    assertTrue(d3B.equals(d4B)) // ! see javadoc

    def d1A: Any = d1
    def d2A: Any = d2
    def d3A: Any = d3
    def d4A: Any = d4
    assertTrue(d1A == d2A)
    assertTrue(d1 == d1A)
    assertTrue(d1A == d1)
    assertTrue(d1B == d1A)
    assertTrue(d1A == d1B)

    assertTrue(d3A != d4A)
    assertTrue(d3 != d4A)
    assertTrue(d3A != d4)
    assertTrue(d3B != d4A)
    assertTrue(d3A != d4B)
  }

  @Test def eqEqJLFloat(): Unit = {
    // Taken from run/sd329.scala in scala/scala

    def f1: Float = 0.0f
    def f2: Float = -0.0f
    def f3: Float = Float.NaN
    def f4: Float = Float.NaN
    assertTrue(f1 == f2)
    assertTrue(f3 != f4)

    def f1B: java.lang.Float = f1
    def f2B: java.lang.Float = f2
    def f3B: java.lang.Float = f3
    def f4B: java.lang.Float = f4
    assertTrueUnlessEqEqJLFloatDoubleBug(f1B == f2B)
    assertTrue(f1 == f1B)
    assertTrue(f1B == f1)
    assertTrueUnlessEqEqJLFloatDoubleBug(f3B != f4B)
    assertTrue(f3 != f4B)
    assertTrue(f3B != f4)

    assertFalse(f1B.equals(f2B)) // ! see javadoc
    assertTrue(f3B.equals(f4B)) // ! see javadoc

    def f1A: Any = f1
    def f2A: Any = f2
    def f3A: Any = f3
    def f4A: Any = f4
    assertTrue(f1A == f2A)
    assertTrue(f1 == f1A)
    assertTrue(f1A == f1)
    assertTrue(f1B == f1A)
    assertTrue(f1A == f1B)

    assertTrue(f3A != f4A)
    assertTrue(f3 != f4A)
    assertTrue(f3A != f4)
    assertTrue(f3B != f4A)
    assertTrue(f3A != f4B)
  }

  @Test def isInstanceOfDoesNotCallToString_Issue2953(): Unit = {
    class C {
      override def toString(): String =
        throw new AssertionError("C.toString must not be called by isInstanceOf")
    }

    @noinline def makeC(): Any = new C

    val c = makeC()

    assertFalse("Boolean", c.isInstanceOf[Boolean])
    assertFalse("Char", c.isInstanceOf[Char])
    assertFalse("Byte", c.isInstanceOf[Byte])
    assertFalse("Short", c.isInstanceOf[Short])
    assertFalse("Int", c.isInstanceOf[Int])
    assertFalse("Long", c.isInstanceOf[Long])
    assertFalse("Float", c.isInstanceOf[Float])
    assertFalse("Double", c.isInstanceOf[Double])
    assertFalse("Unit", c.isInstanceOf[Unit])
    assertFalse("String", c.isInstanceOf[String])
  }

  @Test def superMixinCallIn212_Issue3013(): Unit = {
    assumeTrue(
        "Super mixin calls are broken in Scala/JVM 2.12.{0-2}",
        !Platform.executingInJVM ||
        !Set("2.12.1", "2.12.2").contains(Platform.scalaVersion))

    import Bug3013._

    val b = new B
    val c = new b.C
    assertEquals("A1", c.t1)
    assertEquals("A2", c.t2)
    assertEquals("B", c.t3)
  }

  @Test def tailrecInTraitWithSelfTypeScala212_Issue3058(): Unit = {
    trait Parent { this: Child =>
      @tailrec final def bar(i: Int, acc: Int): Int = {
        if (i <= count)
          bar(i + 1, acc + i)
        else
          acc
      }
    }

    class Child extends Parent {
      def count: Int = 5
    }

    assertEquals(15, new Child().bar(1, 0))
  }

  @Test def tailrecInClassWithSelfTypeScala212_Issue3058(): Unit = {
    class Parent { this: Child =>
      @tailrec final def bar(i: Int, acc: Int): Int = {
        if (i <= count)
          bar(i + 1, acc + i)
        else
          acc
      }
    }

    class Child extends Parent {
      def count: Int = 5
    }

    assertEquals(15, new Child().bar(1, 0))
  }

  @Test def tailrecInTraitWithSelfTypeScala212_Issue3267(): Unit = {
    class Parser {
      def c(): Int = 65
    }

    trait Helpers { this: Parser =>
      @tailrec
      final def rec(i: Int): Int = {
        if (i == 0) b() + c()
        else rec(i - 1)
      }

      def b(): Int = 42
    }

    class ParserWithoutHelpers extends Parser {
      def foo(): Int = 5
    }

    class ParserWithHelpers extends Parser with Helpers

    assertEquals(5, new ParserWithoutHelpers().foo())
    assertEquals(107, new ParserWithHelpers().rec(3))
  }

  @Test def tailrecInClassWithSelfTypeScala212_Issue3267(): Unit = {
    trait Parser {
      def c(): Int = 65
    }

    class Helpers { this: Parser =>
      @tailrec
      final def rec(i: Int): Int = {
        if (i == 0) b() + c()
        else rec(i - 1)
      }

      def b(): Int = 42
    }

    class ParserWithoutHelpers extends Parser {
      def foo(): Int = 5
    }

    class ParserWithHelpers extends Helpers with Parser

    assertEquals(5, new ParserWithoutHelpers().foo())
    assertEquals(107, new ParserWithHelpers().rec(3))
  }

  @Test def adaptedIntToLongInMatch_Issue3281(): Unit = {
    import Bug3281._

    val l: Any = 0 :: Nil
    val r = overloaded(l match {
      case x :: xs => 5
    })
    assertEquals(5L, r)
  }

  @Test def polymorphicArrayApplyWithArrayOfArrayOfChar_Issue3338(): Unit = {
    @inline
    def arrayGet[A](a: Array[A], i: Int): Any = a(i)

    val a = Array(Array('a'))
    val b = arrayGet(a, 0)
    assertTrue(b.isInstanceOf[Array[Char]])
    val c = b.asInstanceOf[Array[Char]]
    val d = arrayGet(c, 0)
    assertTrue(d.isInstanceOf[Char])
    assertEquals('a', d)
  }

  @Test def nestedObjectNamedClass_Issue3888(): Unit = {
    assertEquals(6, `class`.foo(5))
  }

  @Test def gccCrashWithLetConst_Issue4098(): Unit = {
    val set = new java.util.HashSet[String]()
    set.remove("")
    set.remove("1") // only if remove is called twice
    assertEquals(0, set.size())
  }

  @Test def nestedObjectsAndClassesWhoseNamesDifferOnlyInCase_Issue4148(): Unit = {
    // These tests mostly assert that all four objects and classes link
    assertEquals(1, staticForwardersAvoidanceObjectBeforeClass.checkValue)
    assertEquals(2, new StaticForwardersAvoidanceObjectBeforeClass().checkValue)
    assertEquals(3, new StaticForwardersAvoidanceObjectAfterClass().checkValue)
    assertEquals(4, staticForwardersAvoidanceObjectAfterClass.checkValue)
  }

  @Test def fieldsWithNothingType_Issue4370(): Unit = {
    class EagerFieldsWithNothingType {
      val a: Nothing = throw new IllegalStateException("always")
      var b: Nothing = throw new IllegalStateException("never")
    }

    val ex1 = assertThrows(classOf[IllegalStateException], new EagerFieldsWithNothingType)
    assertEquals("always", ex1.getMessage())

    class LazyFieldsWithNothingType {
      lazy val a: Nothing = throw new IllegalStateException("lazily always")
    }

    val obj = new LazyFieldsWithNothingType
    val ex2 = assertThrows(classOf[IllegalStateException], obj.a)
    assertEquals("lazily always", ex2.getMessage())
  }

  @Test def paramDefWithWrongTypeWithHKTAndTypeAliases_Issue3953(): Unit = {
    assumeFalse("Scala/JVM 2.11.x produces wrong bytecode for this test",
        Platform.executingInJVM && Platform.scalaVersion.startsWith("2.11."))

    import scala.language.higherKinds

    sealed class StreamT[M[_]](val step: M[Step[StreamT[M]]])

    sealed abstract class Step[S]

    def mustMatch[A](actual: A)(f: PartialFunction[A, Boolean]): Boolean =
      f.applyOrElse(actual, (_: Any) => false)

    type Id[A] = A

    val result = mustMatch(new StreamT[Id](null).step) {
      case _ => true
    }
    assertTrue(result)
  }

  @Test
  def traitMixinInLocalLazyVal_Issue3918(): Unit = {
    trait TraitMixedInLocalLazyVal {
      val foo = "foobar"
    }

    lazy val localLazyVal = {
      class ClassExtendsTraitInLocalLazyVal extends TraitMixedInLocalLazyVal
      val obj = new ClassExtendsTraitInLocalLazyVal
      obj.foo
    }
    assertEquals("foobar", localLazyVal)
  }

  @Test
  def inferConstableOrConstantDesc_Issue4545(): Unit = {
    /* Depending on the JDK version used to compile this test, the types
     * inferred for the arrays will change. On JDK 12+, they will involve
     * Constable and/or ConstantDesc.
     */

    // On JDK 12+, both Constable and ConstantDesc
    val b = Array("foo", java.lang.Integer.valueOf(5))
    assertEquals(2, b.length)
    assertEquals("foo", b(0))
    assertEquals(5, b(1))

    // On JDK 15+, both Constable but Boolean is not a ConstantDesc
    val a = Array("foo", java.lang.Boolean.TRUE)
    assertEquals(2, a.length)
    assertEquals("foo", a(0))
    assertEquals(true, a(1))
  }

  @Test
  def anyValMethodWithDefaultParamsOverloadedInCompanion_Issue4583(): Unit = {
    assertEquals(5, Bug4583.bar(5))
    assertEquals("hello", Bug4583.bar("hello"))

    val foo = new Bug4583(6)
    assertEquals(6, foo.bar())
    assertEquals(8, foo.bar(2))
  }

  @Test
  def valueCapturedTwiceWithDifferentNames_Issue4716(): Unit = {
    /* The optimizer used to produce Closures with duplicate capture parameter
     * names. This happens when two different vals are captured in a lambda,
     * and these vals are aliases of each other so the optimizer merges them.
     * It then gives the same name to the capture params.
     *
     * To reproduce the bug, we need captures that cannot be eliminated by the
     * emitter afterwards. This is why we need the loop.
     */

    @noinline def hideClosure[A](f: () => A): A = f()

    var done = false
    while (!done) { // don't remove this loop or the test becomes moot
      @noinline def makePair(): (Int, Int) = (5, 6)

      val capture1 = makePair()
      val capture2: scala.Product2[Int, Int] = capture1

      assertEquals(11, hideClosure { () =>
        capture1._1 + capture2._2
      })

      done = true
    }
  }

}

object RegressionTest {
  class Bug218Foo[T](val x: T) extends AnyVal

  class Bug66A(s: String, e: Object) {
    def this(e: Object) = this("", e)
    def this(s: String) = this(s, "")
  }
  class Bug66B(s: String, e: Object) extends Bug66A(s)

  class Bug1955 {
    var result: Int = 0

    def doSomething[A](a: Int, b: Int, r: A): A = {
      result = a + b
      r
    }

    def bug(x: Int, e: Boolean): Unit = {
      x match {
        case 1 => doSomething(123, 456, ())
        case 2 if e =>
      }

      if (false) ()
    }
  }

  object Bug3013 {
    trait A1 {
      private val s = "A1"
      def f: String = s
    }

    trait A2 {
      private val s = "A2"
      def f: String = s
    }

    class B extends A1 with A2 {
      override def f: String = "B"

      class C {
        def t1: String = B.super[A1].f
        def t2: String = B.super[A2].f
        def t3: String = B.this.f
      }
    }
  }

  object Bug3281 {
    def overloaded(x: Long): Any =
      x

    def overloaded(x: Any): Unit =
      fail("Bug3281.overloaded(x: Any) was called")
  }

  object `class` { // scalastyle:ignore
    def foo(x: Int): Int = x + 1
  }

  class Bug4583(val x: Int) extends AnyVal {
    def bar(y: Int = 0): Int = x + y
  }

  object Bug4583 {
    def bar(x: Int): Int = x
    def bar(x: String): String = x
  }

  /* The objects and classes here intentionally have names that differ only in
   * case, and are intentionally defined in a specific order. This is required
   * to properly test the fix for #4148 (static forwarders can overwrite
   * companion classes with a name that differs only in case on
   * case-insensitive file systems). Depending on the order of which comes
   * first or second, different strategies can fail, so we test both. For
   * example, prior to the fix, #4148 would only manifest itself when the
   * object was declared *after* the class, but not before.
   */

  object staticForwardersAvoidanceObjectBeforeClass {
    def checkValue: Int = 1
  }

  class StaticForwardersAvoidanceObjectBeforeClass {
    def checkValue: Int = 2
  }

  class StaticForwardersAvoidanceObjectAfterClass {
    def checkValue: Int = 3
  }

  object staticForwardersAvoidanceObjectAfterClass {
    def checkValue: Int = 4
  }
}
