/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import scala.annotation.tailrec

import scala.scalajs.js

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

import org.scalajs.testsuite.utils.Platform

class RegressionTest {
  import RegressionTest._

  @Test def `Wrong_division_conversion_(7_/_2.0)_issue_18`(): Unit = {
    val div = 7 / 2.0
    assertEquals(3.5, div)
    assertEquals("double", div.getClass.getName)

    val mod = 7 % 2.0
    assertEquals(1.0, mod)
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

  @Test def String_concatenation_with_null_issue_26(): Unit = {
    val x: Object = null
    assertEquals("nullcheck", x + "check")
  }

  @Test def should_emit_static_calls_when_forwarding_to_another_constructor_issue_66(): Unit = {
    new Bug66B("", "")
  }

  @Test def should_not_swallow_Unit_expressions_when_converting_to_js_Any_issue_83(): Unit = {
    var effectHappened = false
    def doEffect(): Unit = effectHappened = true
    def f(): js.Any = doEffect()
    f()
    assertTrue(effectHappened)
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

  @Test def should_resolve_overloads_on_scala_Function_apply_when_converting_to_js_Function_issue_125(): Unit = {
    class Fct extends Function1[Int, Any] {
      def apply(n: Int): Int = n
    }

    val scalaFunction = new Fct
    val jsFunction: js.Any = scalaFunction
    val thisFunction: js.ThisFunction = scalaFunction
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
    assertEquals(0.0f, float)

    val double = zero[Double]
    assertTrue((double: Any).isInstanceOf[Double])
    assertEquals(0.0, double)

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

    def giveMeANothing(): Nothing = sys.error("boom")
    assertThrows(classOf[Exception], (giveMeANothing(): StringBuilder).append(5))
    assertThrows(classOf[Exception], (giveMeANothing(): scala.runtime.IntRef).elem)
  }

  @Test def should_not_put_bad_flags_on_caseaccessor_export_forwarders_issue_1191(): Unit = {
    // This test used to choke patmat

    @scala.scalajs.js.annotation.JSExportAll
    case class T(one: Int, two: Int)

    val T(a, b) = T(1, 2)

    assertEquals(1, a)
    assertEquals(2, b)
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

  @Test def should_support_debugger_statements_through_the_whole_pipeline_issue_1402(): Unit = {
    // A function that hopfully persuades the optimizer not to optimize
    // we need a debugger statement that is unreachable, but not eliminated
    @noinline
    class A(var z: Int = 4) {
      var x: Int = _
      var y: Int = _

      @noinline
      def plus(x0: Int, y0: Int): Int = {
        x = x0
        y = y0
        var res = 0
        while (x > 0 || y > 0 || z > 0) {
          if (x > 0) x -= 1
          else if (y > 0) y -= 1
          else z -= 1
          res += 1
        }
        res
      }
    }

    if (new A().plus(5, 10) < 3)
      js.debugger()
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
        assertEquals(5, x.asInstanceOf[js.Any])
    }

    // Make sure the specialized Function1.apply(Double)Double is reachable.
    @noinline def makeFun(y: Double): Double => Double = {
      val z = y + 1.5
      ((x: Double) => x * z): (Double => Double)
    }
    val someDoubleFun = makeFun(2.0)
    assertEquals(147.0, someDoubleFun(42.0))

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
}
