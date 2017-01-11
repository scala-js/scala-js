/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import language.reflectiveCalls

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform
import org.scalajs.testsuite.utils.AssertThrows._

import java.lang.{Float => JFloat, Double => JDouble}

class ReflectiveCallTest {
  import ReflectiveCallTest._

  @Test def should_allow_subtyping_in_return_types(): Unit = {
    class A { def x: Int = 1 }
    class B extends A { override def x: Int = 2 }

    object Generator {
      def generate(): B = new B
    }

    def f(x: { def generate(): A }): A = x.generate

    assertEquals(2, f(Generator).x)
  }

  @Test def should_allow_this_type_in_return_types(): Unit = {
    type ValueType = { def value: this.type }
    def f(x: ValueType): ValueType = x.value

    class StringValue(x: String) {
      def value: this.type = this
      override def toString(): String = s"StringValue($x)"
    }

    assertEquals("StringValue(foo)", f(new StringValue("foo")).toString)
  }

  @Test def should_allow_generic_return_types(): Unit = {
    case class Tata(name: String)

    object Rec {
      def e(x: Tata): Tata = new Tata("iei")
    }

    def m[T](r: Object { def e(x: Tata): T}): T =
      r.e(new Tata("foo"))

    assertEquals("Tata(iei)", m[Tata](Rec).toString)
  }

  @Test def should_work_with_unary_methods_on_primitive_types(): Unit = {
    // scalastyle:off disallow.space.before.token
    def fInt(x: Any { def unary_- : Int }): Int = -x
    assertEquals(-1, fInt(1.toByte))
    assertEquals(-1, fInt(1.toShort))
    assertEquals(-1, fInt(1.toChar))
    assertEquals(-1, fInt(1))

    def fLong(x: Any { def unary_- : Long }): Long = -x
    assertEquals(-1L, fLong(1L))

    def fFloat(x: Any { def unary_- : Float}): Float = -x
    assertEquals(-1.5f, fFloat(1.5f), 1e-5f)

    def fDouble(x: Any { def unary_- : Double }): Double = -x
    assertEquals(-1.5, fDouble(1.5), 1e-5d)

    def fBoolean(x: Any { def unary_! : Boolean }): Boolean = !x
    assertTrue(fBoolean(false))
    assertFalse(fBoolean(true))
    // scalastyle:on disallow.space.before.token
  }

  @Test def should_work_with_binary_operators_on_primitive_types(): Unit = {
    def fLong(x: Any { def +(x: Long): Long }): Long = x + 5L
    assertEquals(10L, fLong(5.toByte))
    assertEquals(15L, fLong(10.toShort))
    assertEquals(15L, fLong(10.toChar))
    assertEquals(4L, fLong(-1))
    assertEquals(22L, fLong(17L))

    def fInt(x: Any { def /(x: Int): Int }): Int = x / 7
    assertEquals(9, fInt(65.toByte))
    assertEquals(2, fInt(15.toShort))
    assertEquals(3, fInt(25.toChar))
    assertEquals(-5, fInt(-40))

    def fShort(x: Any { def +(x: Short): Int }): Int = x + 6.toShort
    assertEquals(71, fShort(65.toByte))
    assertEquals(21, fShort(15.toShort))
    assertEquals(31, fShort(25.toChar))
    assertEquals(-34, fShort(-40))

    def fFloat(x: Any { def %(x: Float): Float}): Float = x % 3.4f
    assertEquals(2.1f, fFloat(5.5f), 1e-5f)

    def fDouble(x: Any { def /(x: Double): Double }): Double = x / 1.4
    assertEquals(-1.0714285714285714, fDouble(-1.5), 1e-5d)

    def fBoolean(x: Any { def &&(x: Boolean): Boolean }): Boolean = x && true // scalastyle:ignore
    assertFalse(fBoolean(false))
    assertTrue(fBoolean(true))
  }

  @Test def should_work_with_equality_operators_on_primitive_types(): Unit = {
    assumeFalse("Reflective call to == and != is broken on the JVM",
        Platform.executingInJVM)

    def fNum(obj: Any { def ==(x: Int): Boolean }): Boolean = obj == 5
    assertTrue(fNum(5.toByte))
    assertFalse(fNum(6.toByte))
    assertTrue(fNum(5.toShort))
    assertFalse(fNum(7.toShort))
    assertTrue(fNum(5.toChar))
    assertFalse(fNum('r'))
    assertTrue(fNum(5))
    assertFalse(fNum(-4))
    assertTrue(fNum(5L))
    assertFalse(fNum(400L))
    assertTrue(fNum(5.0f))
    assertFalse(fNum(5.6f))
    assertTrue(fNum(5.0))
    assertFalse(fNum(7.9))

    def fBool(obj: Any { def ==(x: Boolean): Boolean }): Boolean = obj == false // scalastyle:ignore
    assertFalse(fBool(true))
    assertTrue(fBool(false))

    def fNumN(obj: Any { def !=(x: Int): Boolean }): Boolean = obj != 5
    assertFalse(fNumN(5.toByte))
    assertTrue(fNumN(6.toByte))
    assertFalse(fNumN(5.toShort))
    assertTrue(fNumN(7.toShort))
    assertFalse(fNumN(5.toChar))
    assertTrue(fNumN('r'))
    assertFalse(fNumN(5))
    assertTrue(fNumN(-4))
    assertFalse(fNumN(5L))
    assertTrue(fNumN(400L))
    assertFalse(fNumN(5.0f))
    assertTrue(fNumN(5.6f))
    assertFalse(fNumN(5.0))
    assertTrue(fNumN(7.9))

    def fBoolN(obj: Any { def !=(x: Boolean): Boolean }): Boolean = obj != false // scalastyle:ignore
    assertTrue(fBoolN(true))
    assertFalse(fBoolN(false))
  }

  @Test def should_work_with_Arrays(): Unit = {
    type UPD = { def update(i: Int, x: String): Unit }
    type APL = { def apply(i: Int): String }
    type LEN = { def length: Int }
    type CLONE = Any { def clone(): Object }

    def upd(obj: UPD, i: Int, x: String): Unit = obj.update(i,x)
    def apl(obj: APL, i: Int): String = obj.apply(i)
    def len(obj: LEN): Int = obj.length
    def clone(obj: CLONE): Object = obj.clone

    val x = Array("asdf","foo","bar")
    val y = clone(x).asInstanceOf[Array[String]]

    assertEquals(3, len(x))
    assertEquals("asdf", apl(x,0))
    upd(x,1,"2foo")
    assertEquals("2foo", x(1))
    assertEquals("foo", y(1))
  }

  @Test def should_work_with_Arrays_of_primitive_values(): Unit = {
    type UPD = { def update(i: Int, x: Int): Unit }
    type APL = { def apply(i: Int): Int}
    type LEN = { def length: Int }
    type CLONE = Any { def clone(): Object }

    def upd(obj: UPD, i: Int, x: Int): Unit = obj.update(i,x)
    def apl(obj: APL, i: Int): Int = obj.apply(i)
    def len(obj: LEN): Int = obj.length
    def clone(obj: CLONE): Object = obj.clone

    val x = Array(5,2,8)
    val y = clone(x).asInstanceOf[Array[Int]]

    assertEquals(3, len(x))
    assertEquals(5, apl(x,0))
    upd(x,1,1000)
    assertEquals(1000, x(1))
    assertEquals(2, y(1))
  }

  @Test def should_work_with_Strings(): Unit = {
    def get(obj: { def codePointAt(str: Int): Int }): Int =
      obj.codePointAt(1)
    assertEquals('i'.toInt, get("Hi"))

    def sub(x: { def substring(x: Int): AnyRef }): AnyRef = x.substring(5)
    assertEquals("sdfasdf", sub("asdfasdfasdf"))

    type LEN_A = { def length: Any }
    def lenA(x: LEN_A): Any = x.length
    assertEquals(4, lenA("asdf"))
  }

  @Test def should_properly_generate_forwarders_for_inherited_methods(): Unit = {
    trait A {
      def foo: Int
    }

    abstract class B extends A

    class C extends B {
      def foo: Int = 1
    }

    def call(x: { def foo: Int }): Int = x.foo

    assertEquals(1, call(new C))
  }

  @Test def should_be_bug_compatible_with_Scala_JVM_for_inherited_overloads(): Unit = {
    class Base {
      def foo(x: Option[Int]): String = "a"
    }

    class Sub extends Base {
      def foo(x: Option[String]): Int = 1
    }

    val sub = new Sub

    val x: { def foo(x: Option[Int]): Any } = sub
    assertEquals(1, x.foo(Some(1))) // here is the "bug"

    val y: { def foo(x: Option[String]): Any } = sub
    assertEquals(1, y.foo(Some("hello")))
  }

  @Test def should_work_on_java_lang_Object_notify_notifyAll_issue_303(): Unit = {
    type ObjNotifyLike = Any {
      def notify(): Unit
      def notifyAll(): Unit
    }
    def objNotifyTest(obj: ObjNotifyLike): Int = {
      obj.notify()
      obj.notifyAll()
      1
    }

    class A

    val a = new A()
    a.synchronized {
      assertEquals(1, objNotifyTest(a))
    }
  }

  @Test def should_work_on_java_lang_Object_clone_issue_303(): Unit = {
    type ObjCloneLike = Any { def clone(): AnyRef }
    def objCloneTest(obj: ObjCloneLike): AnyRef = obj.clone()

    class B(val x: Int) extends Cloneable {
      override def clone(): AnyRef = super.clone()
    }

    val b = new B(1)
    val bClone = objCloneTest(b).asInstanceOf[B]

    assertFalse(b eq bClone)
    assertEquals(1, bClone.x)
  }

  @Test def should_not_work_on_scala_AnyRef_eq_ne_synchronized_issue_2709(): Unit = {
    // Bug compatible with Scala/JVM

    assumeFalse(
        "GCC is a bit too eager in its optimizations in this error case",
        Platform.isInFullOpt)

    type ObjWithAnyRefPrimitives = Any {
      def eq(that: AnyRef): Boolean
      def ne(that: AnyRef): Boolean
      def synchronized[T](f: T): Any
    }

    def objEqTest(obj: ObjWithAnyRefPrimitives, that: AnyRef): Boolean =
      obj eq that
    def objNeTest(obj: ObjWithAnyRefPrimitives, that: AnyRef): Boolean =
      obj ne that
    def objSynchronizedTest(obj: ObjWithAnyRefPrimitives, f: String): Any =
      obj.synchronized(f)

    /* The name of the expected exception class. We cannot use
     * classOf[js.JavaScriptException] as that would not compile on the JVM.
     */
    val expectedClassName =
      if (Platform.executingInJVM) "java.lang.NoSuchMethodException"
      else "scala.scalajs.js.JavaScriptException"

    def testWith(body: => Unit): Unit = {
      val exception = expectThrows(classOf[Throwable], body)
      assertEquals(expectedClassName, exception.getClass.getName)
    }

    class A

    val a1 = new A
    val a2 = new A

    testWith(objEqTest(a1, a2))
    testWith(objNeTest(a1, a2))
    testWith(objSynchronizedTest(a1, "hello"))
  }

  @Test def should_work_on_AnyVal_eq_ne_synchronized_issue_2709(): Unit = {
    type ObjWithAnyRefPrimitives = Any {
      def eq(that: AnyRef): Boolean
      def ne(that: AnyRef): Boolean
      def synchronized[T](f: T): Any
    }

    def objEqTest(obj: ObjWithAnyRefPrimitives, that: AnyRef): Boolean =
      obj eq that
    def objNeTest(obj: ObjWithAnyRefPrimitives, that: AnyRef): Boolean =
      obj ne that
    def objSynchronizedTest(obj: ObjWithAnyRefPrimitives, f: String): Any =
      obj.synchronized(f)

    val a = new AnyValWithAnyRefPrimitiveMethods(5)

    assertFalse(objEqTest(a, 5: Integer))
    assertTrue(objEqTest(a, 6: Integer))

    assertTrue(objNeTest(a, 5: Integer))
    assertFalse(objNeTest(a, 6: Integer))

    assertEquals("hellothere", objSynchronizedTest(a, "hello"))
  }

  @Test def should_work_on_java_lang_Float_Double_isNaN_isInfinite(): Unit = {
    type FloatingNumberLike = Any {
      def isNaN(): Boolean
      def isInfinite(): Boolean
    }
    def test(x: FloatingNumberLike, isNaN: Boolean,
        isInfinite: Boolean): Unit = {
      assertEquals(isNaN, x.isNaN())
      assertEquals(isInfinite, x.isInfinite())
    }

    test(new JFloat(Float.NaN), true, false)
    test(new JFloat(Float.PositiveInfinity), false, true)
    test(new JFloat(Float.NegativeInfinity), false, true)
    test(new JFloat(54.67), false, false)

    test(new JDouble(Double.NaN), true, false)
    test(new JDouble(Double.PositiveInfinity), false, true)
    test(new JDouble(Double.NegativeInfinity), false, true)
    test(new JDouble(54.67), false, false)
  }

  @Test def should_work_with_default_arguments_issue_390(): Unit = {
    def pimpIt(a: Int) = new { // scalastyle:ignore
      def foo(b: Int, c: Int = 1): Int = a + b + c
    }

    assertEquals(4, pimpIt(1).foo(2))
    assertEquals(8, pimpIt(2).foo(2,4))
  }

  @Test def should_unbox_all_types_of_arguments_issue_899(): Unit = {
    class Foo {
      def makeInt: Int = 5
      def testInt(x: Int): Unit = assertEquals(5, x)

      def makeRef: Option[String] = Some("hi")
      def testRef(x: Option[String]): Unit = assertEquals(Some("hi"), x)
    }

    /* Note: we should also test with value classes, except that Scala itself
     * does not support value classes as parameters or result type of
     * methods in structural types.
     */

    def test(foo: {
      def makeInt: Int
      def testInt(x: Int): Unit
      def makeRef: Option[String]
      def testRef(x: Option[String]): Unit
    }): Unit = {
      foo.testInt(foo.makeInt)
      foo.testRef(foo.makeRef)
    }

    test(new Foo)
  }
}

object ReflectiveCallTest {
  class AnyValWithAnyRefPrimitiveMethods(val x: Int) extends AnyVal {
    def eq(that: AnyRef): Boolean = (x + 1) == that
    def ne(that: AnyRef): Boolean = (x + 1) != that
    def synchronized[T](f: T): Any = f + "there"
  }
}
