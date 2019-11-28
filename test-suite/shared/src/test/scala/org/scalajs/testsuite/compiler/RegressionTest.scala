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

import scala.annotation.tailrec

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows._

import org.scalajs.testsuite.utils.Platform

class RegressionTest {
  import RegressionTest._

  @Test def `Wrong_division_conversion_(7_/_2.0)_issue_18`(): Unit = {
    val div = 7 / 2.0
    assertEquals(3.5, div, 0.0)
    assertEquals("double", div.getClass.getName)

    val mod = 7 % 2.0
    assertEquals(1.0, mod, 0.0)
    assertEquals("double", mod.getClass.getName)
  }

  @Test def Abort_with_some_pattern_match_guards_issue_22(): Unit = {
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

  @Test def Bad_encoding_for_characters_spanning_2_UTF_16_chars_issue_23(): Unit = {
    val str = "A∀\uD835\uDCAB"
    var s: String = ""
    for (c <- str) {
      val code: Int = c
      s = s + code + " "
    }
    assertEquals("65 8704 55349 56491 ", s)
  }

  @Test def characterEscapes_issue_3125(): Unit = {
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

  @Test def String_concatenation_with_null_issue_26(): Unit = {
    val x: Object = null
    assertEquals("nullcheck", x + "check")
  }

  @Test def should_emit_static_calls_when_forwarding_to_another_constructor_issue_66(): Unit = {
    new Bug66B("", "")
  }

  @Test def should_correctly_call_subSequence_on_non_string_CharSequences_issue_55(): Unit = {
    val arr: CharSequence = Array('a','b','c','d')
    val ss = arr.subSequence(2,3)
    assertEquals(1, ss.length())
    assertEquals('c', ss.charAt(0))
  }

  @Test def should_correctly_concat_primitive_values_to_strings_issue_113(): Unit = {
    assertEquals("4foo", 4 + "foo")
    assertEquals("afoo", 'a' + "foo")
  }

  @Test def should_correctly_dispatch_calls_on_private_functions_issue_165(): Unit = {
    class A {
      private def x: Int = 1
      def value: Int = x
    }
    class B extends A {
      private def x: Int = 2
    }
    assertEquals(1, new B().value)
  }

  @Test def should_correctly_mangle_JavaScript_reserved_identifiers_issue_153(): Unit = {
    // scalastyle:off class.name

    // Class name
    class break {
      // class variable
      var continue: Int = 1
      // method name
      def switch: Int = {
        // local name
        val default = 2
        default
      }
    }
    trait Foo {
      // static member (through mixin)
      def function: Int = 3
    }

    val x = new break with Foo
    assertEquals(1, x.continue)
    assertEquals(2, x.switch)
    assertEquals(3, x.function)

    // scalastyle:on class.name
  }

  @Test def should_correctly_mangle_identifiers_starting_with_a_digit_issue_153(): Unit = {
    // scalastyle:off class.name

    // Class name
    class `0` {
      // class variable
      var `1`: Int = 1
      // method name
      def `2`: Int = {
        // local name
        val `22` = 2
        `22`
      }
    }
    trait Foo {
      // static member (through mixin)
      def `3`: Int = 3
    }

    val x = new `0` with Foo
    assertEquals(1, x.`1`)
    assertEquals(2, x.`2`)
    assertEquals(3, x.`3`)

    // scalastyle:on class.name
  }

  @Test def should_reserve_eval_and_arguments_issue_743(): Unit = {
    val eval = 5
    assertEquals(5, eval)
    val arguments = "hello"
    assertEquals("hello", arguments)
  }

  @Test def should_support_class_literals_for_existential_value_types_issue_218(): Unit = {
    import Platform.scalaVersion

    assumeFalse("Affected by https://github.com/scala/bug/issues/10551",
        Platform.executingInJVM && {
          scalaVersion.startsWith("2.10.") ||
          scalaVersion.startsWith("2.11.") ||
          scalaVersion == "2.12.0" || scalaVersion == "2.12.1" ||
          scalaVersion == "2.12.2" || scalaVersion == "2.12.3" ||
          scalaVersion == "2.12.4"
        })

    assertEquals("org.scalajs.testsuite.compiler.RegressionTest$Bug218Foo",
        scala.reflect.classTag[Bug218Foo[_]].toString)
  }

  @Test def should_support_Buffer_issue_268(): Unit = {
    val a = scala.collection.mutable.Buffer.empty[Int]
    a.insert(0, 0)
    a.remove(0)
    for (i <- 0 to 10) {
      a.insert(a.length / 2, i)
    }
    assertEquals("1, 3, 5, 7, 9, 10, 8, 6, 4, 2, 0", a.mkString(", "))
  }

  @Test def should_not_call_equals_when_comparing_with_a_literal_null_issue_362(): Unit = {
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

  @Test def should_unbox_null_to_the_zero_of_types_issue_674(): Unit = {
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

  @Test def Param_defs_in_tailrec_methods_should_be_considered_mutable_issue_825(): Unit = {
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

  @Test def null_synchronized_should_throw_issue_874(): Unit = {
    assertThrows(classOf[NullPointerException], null.synchronized(5))
  }

  @Test def x_synchronized_should_preserve_side_effects_of_x(): Unit = {
    var c = 0
    def x: RegressionTest.this.type = { c += 1; this }
    assertEquals(5, x.synchronized(5))
    assertEquals(1, c)
  }

  @Test def IR_checker_should_allow_Apply_Select_on_NullType_and_NothingType_issue_1123(): Unit = {
    def giveMeANull(): Null = null
    assertThrows(classOf[Exception], (giveMeANull(): StringBuilder).append(5))
    assertThrows(classOf[Exception], (giveMeANull(): scala.runtime.IntRef).elem)

    def giveMeANothing(): Nothing = throw new Exception("boom")
    assertThrows(classOf[Exception], (giveMeANothing(): StringBuilder).append(5))
    assertThrows(classOf[Exception], (giveMeANothing(): scala.runtime.IntRef).elem)
  }

  @Test def IR_checker_must_not_check_field_existence_on_non_existent_classes(): Unit = {
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

  @Test def IR_checker_must_not_check_field_existence_on_classes_with_no_instance_issue_3060(): Unit = {
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

  @Test def should_properly_order_ctor_statements_when_inlining_issue_1369(): Unit = {
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

  @Test def should_not_restrict_mutability_of_fields_issue_1021(): Unit = {
    class A {
      /* This var is refered to in the lambda passed to `foreach`. Therefore
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

  @Test def should_populate_desugar_environments_with_Closure_params_issue_1399(): Unit = {
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

  @Test def should_not_cause_Closure_to_crash_with_Unexpected_variable_NaN_issue_1469(): Unit = {
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

  @Test def switch_match_with_2_guards_for_the_same_value_issue_1589(): Unit = {
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

  @Test def switch_match_with_a_guard_and_a_result_type_of_BoxedUnit_issue_1955(): Unit = {
    val bug = new Bug1955
    bug.bug(2, true)
    assertEquals(0, bug.result)
    bug.bug(1, true)
    assertEquals(579, bug.result)
    assertThrows(classOf[MatchError], bug.bug(2, false))
  }

  @Test def return_x_match_issue_2928(): Unit = {
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

  @Test def null_asInstanceOf_Unit_should_succeed_issue_1691(): Unit = {
    /* Avoid scalac's special treatment of `<literal null>.asInstanceOf[X]`.
     * It does have the benefit to test our constant-folder of that pattern,
     * once getNull() is inlined; and of our run-time implementation, when the
     * optimizer is disabled.
     */
    def getNull(): Any = null
    val x = getNull().asInstanceOf[Unit]: Any

    val scalaVersion = Platform.scalaVersion
    if (scalaVersion.startsWith("2.10.") || scalaVersion.startsWith("2.11.")) {
      assertNull(x.asInstanceOf[AnyRef])
    } else {
      // As of Scala 2.12.0-M5, null.asInstanceOf[Unit] (correctly) returns ()
      assertEquals((), x)
    }
  }

  @Test def lambda_parameter_with_a_dash_issue_1790(): Unit = {
    val f = (`a-b`: Int) => `a-b` + 1
    assertEquals(6, f(5))
  }

  @Test def nested_labeled_block_sort_circuit_returns_issue_2307(): Unit = {
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

    {
      v.startsWith("2.10.") ||
      v.startsWith("2.11.") ||
      v == "2.12.0" ||
      v == "2.12.1"
    }
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

  @Test def isInstanceOf_must_not_call_toString_issue_2953(): Unit = {
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

  @Test def super_mixin_call_in_2_12_issue_3013(): Unit = {
    assumeTrue(
        "Super mixin calls are broken in Scala/JVM 2.12.{0-2}",
        !Platform.executingInJVM ||
        !Set("2.12.0", "2.12.1", "2.12.2").contains(Platform.scalaVersion))

    import Bug3013._

    val b = new B
    val c = new b.C
    assertEquals("A1", c.t1)
    assertEquals("A2", c.t2)
    assertEquals("B", c.t3)
  }

  @Test def tailrec_in_trait_with_self_type_scala_2_12_issue_3058(): Unit = {
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

  @Test def tailrec_in_class_with_self_type_scala_2_12_issue_3058(): Unit = {
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

  @Test def tailrec_in_trait_with_self_type_scala_2_12_issue_3267(): Unit = {
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

  @Test def tailrec_in_class_with_self_type_scala_2_12_issue_3267(): Unit = {
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

  @Test def adaptedIntToLongInMatch_issue_3281(): Unit = {
    import Bug3281._

    val l: Any = 0 :: Nil
    val r = overloaded(l match {
      case x :: xs => 5
    })
    assertEquals(5L, r)
  }

  @Test def polymorphicArrayApplyWithArrayOfArrayOfChar_issue_3338(): Unit = {
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

  @Test def nested_object_named_class_issue_3888(): Unit = {
    assertEquals(6, `class`.foo(5))
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
}
