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

import language.reflectiveCalls

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform
import org.scalajs.testsuite.utils.AssertThrows.assertThrows

import java.lang.{Float => JFloat, Double => JDouble}

class ReflectiveCallTest {
  import ReflectiveCallTest._

  @Test def subtypingInReturnTypes(): Unit = {
    class A { def x: Int = 1 }
    class B extends A { override def x: Int = 2 }

    object Generator {
      def generate(): B = new B
    }

    def f(x: { def generate(): A }): A = x.generate

    assertEquals(2, f(Generator).x)
  }

  @Test def thisTypeInReturnTypes(): Unit = {
    type ValueType = { def value: this.type }
    def f(x: ValueType): ValueType = x.value

    class StringValue(x: String) {
      def value: this.type = this
      override def toString(): String = s"StringValue($x)"
    }

    assertEquals("StringValue(foo)", f(new StringValue("foo")).toString)
  }

  @Test def genericReturnTypes(): Unit = {
    case class Tata(name: String)

    object Rec {
      def e(x: Tata): Tata = new Tata("iei")
    }

    def m[T](r: Object { def e(x: Tata): T }): T =
      r.e(new Tata("foo"))

    assertEquals("Tata(iei)", m[Tata](Rec).toString)
  }

  @Test def unaryMethodsOnPrimitiveTypes(): Unit = {
    def fInt(x: Any { def unary_- : Int }): Int = -x
    assertEquals(-1, fInt(1.toByte))
    assertEquals(-1, fInt(1.toShort))
    assertEquals(-1, fInt(1.toChar))
    assertEquals(-1, fInt(1))

    def fLong(x: Any { def unary_- : Long }): Long = -x
    assertEquals(-1L, fLong(1L))

    def fFloat(x: Any { def unary_- : Float }): Float = -x
    assertEquals(-1.5f, fFloat(1.5f), 1e-5f)

    def fDouble(x: Any { def unary_- : Double }): Double = -x
    assertEquals(-1.5, fDouble(1.5), 1e-5d)

    def fBoolean(x: Any { def unary_! : Boolean }): Boolean = !x
    assertTrue(fBoolean(false))
    assertFalse(fBoolean(true))
  }

  @Test def binaryOperatorsOnPrimitiveTypes(): Unit = {
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

    def fFloat(x: Any { def %(x: Float): Float }): Float = x % 3.4f
    assertEquals(2.1f, fFloat(5.5f), 1e-5f)

    def fDouble(x: Any { def /(x: Double): Double }): Double = x / 1.4
    assertEquals(-1.0714285714285714, fDouble(-1.5), 1e-5d)

    def fBoolean(x: Any { def &&(x: Boolean): Boolean }): Boolean = x && true // scalastyle:ignore
    assertFalse(fBoolean(false))
    assertTrue(fBoolean(true))
  }

  @Test def qualityOperatorsOnPrimitiveTypes(): Unit = {
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

  @Test def compareToForPrimitives(): Unit = {
    def fCompareToBoolean(x: { def compareTo(y: java.lang.Boolean): Int }, y: Boolean): Int =
      x.compareTo(y)
    assertTrue(fCompareToBoolean(false, true) < 0)

    def fCompareToChar(x: { def compareTo(y: java.lang.Character): Int }, y: Char): Int =
      x.compareTo(y)
    assertTrue(fCompareToChar('A', 'C') < 0)

    def fCompareToByte(x: { def compareTo(y: java.lang.Byte): Int }, y: Byte): Int =
      x.compareTo(y)
    assertTrue(fCompareToByte(5.toByte, 6.toByte) < 0)

    def fCompareToShort(x: { def compareTo(y: java.lang.Short): Int }, y: Short): Int =
      x.compareTo(y)
    assertTrue(fCompareToShort(5.toShort, 6.toShort) < 0)

    def fCompareToInt(x: { def compareTo(y: java.lang.Integer): Int }, y: Int): Int =
      x.compareTo(y)
    assertTrue(fCompareToInt(5, 6) < 0)

    def fCompareToLong(x: { def compareTo(y: java.lang.Long): Int }, y: Long): Int =
      x.compareTo(y)
    assertTrue(fCompareToLong(5L, 6L) < 0)

    def fCompareToFloat(x: { def compareTo(y: java.lang.Float): Int }, y: Float): Int =
      x.compareTo(y)
    assertTrue(fCompareToFloat(5.5f, 6.5f) < 0)

    def fCompareToDouble(x: { def compareTo(y: java.lang.Double): Int }, y: Double): Int =
      x.compareTo(y)
    assertTrue(fCompareToDouble(5.5, 6.5) < 0)
  }

  @Test def concatForPrimitives(): Unit = {
    // See https://github.com/scala/bug/issues/10469
    assumeFalse("Reflective call prim.+(String) broken on the JVM",
        Platform.executingInJVM)

    def concat(x: Any { def +(y: String): String }, y: String): String = x + y

    assertEquals("truefoo", concat(true, "foo"))
    assertEquals("Afoo", concat('A', "foo"))
    assertEquals("5foo", concat(5.toByte, "foo"))
    assertEquals("5foo", concat(5.toShort, "foo"))
    assertEquals("5foo", concat(5, "foo"))
    assertEquals("5foo", concat(5L, "foo"))
    assertEquals("5.5foo", concat(5.5f, "foo"))
    assertEquals("5.5foo", concat(5.5, "foo"))
  }

  @Test def arrays(): Unit = {
    type UPD = { def update(i: Int, x: String): Unit }
    type APL = { def apply(i: Int): String }
    type LEN = { def length: Int }
    type CLONE = Any { def clone(): Object }

    def upd(obj: UPD, i: Int, x: String): Unit = obj.update(i, x)
    def apl(obj: APL, i: Int): String = obj.apply(i)
    def len(obj: LEN): Int = obj.length
    def clone(obj: CLONE): Object = obj.clone

    val x = Array("asdf", "foo", "bar")
    val y = clone(x).asInstanceOf[Array[String]]

    assertEquals(3, len(x))
    assertEquals("asdf", apl(x, 0))
    upd(x, 1, "2foo")
    assertEquals("2foo", x(1))
    assertEquals("foo", y(1))
  }

  @Test def arraysOfPrimitiveValues(): Unit = {
    type UPD = { def update(i: Int, x: Int): Unit }
    type APL = { def apply(i: Int): Int }
    type LEN = { def length: Int }
    type CLONE = Any { def clone(): Object }

    def upd(obj: UPD, i: Int, x: Int): Unit = obj.update(i, x)
    def apl(obj: APL, i: Int): Int = obj.apply(i)
    def len(obj: LEN): Int = obj.length
    def clone(obj: CLONE): Object = obj.clone

    val x = Array(5, 2, 8)
    val y = clone(x).asInstanceOf[Array[Int]]

    assertEquals(3, len(x))
    assertEquals(5, apl(x, 0))
    upd(x, 1, 1000)
    assertEquals(1000, x(1))
    assertEquals(2, y(1))
  }

  @Test def strings(): Unit = {
    def get(obj: { def codePointAt(str: Int): Int }): Int =
      obj.codePointAt(1)
    assertEquals('i'.toInt, get("Hi"))

    def sub(x: { def substring(x: Int): AnyRef }): AnyRef = x.substring(5)
    assertEquals("sdfasdf", sub("asdfasdfasdf"))

    type LEN_A = { def length: Any }
    def lenA(x: LEN_A): Any = x.length
    assertEquals(4, lenA("asdf"))

    def compareToString(x: { def compareTo(y: String): Int }, y: String): Int =
      x.compareTo(y)
    assertTrue(compareToString("hello", "world") < 0)
  }

  @Test def forwardersForInheritedMethods(): Unit = {
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

  @Test def bugCompatibleWithScalaJVMForInheritedOverloads(): Unit = {
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

  @Test def javaLangObjectNotifyNotifyAll_Issue303(): Unit = {
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

  @Test def javaLangObjectClone_Issue303(): Unit = {
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

  @Test def scalaAnyRefEqNeSynchronized_Issue2709(): Unit = {
    // Bug compatible with Scala/JVM

    assumeFalse(
        "GCC is a bit too eager in its optimizations in this error case",
        Platform.usesClosureCompiler)

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
      val exception = assertThrows(classOf[Throwable], body)
      assertEquals(expectedClassName, exception.getClass.getName)
    }

    class A

    val a1 = new A
    val a2 = new A

    testWith(objEqTest(a1, a2))
    testWith(objNeTest(a1, a2))
    testWith(objSynchronizedTest(a1, "hello"))
  }

  @Test def anyValEqNeSynchronized_Issue2709(): Unit = {
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

  @Test def javaLangFloatDoubleIsNaNIsInfinite(): Unit = {
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

  @Test def defaultArguments_Issue390(): Unit = {
    def pimpIt(a: Int) = new { // scalastyle:ignore
      def foo(b: Int, c: Int = 1): Int = a + b + c
    }

    assertEquals(4, pimpIt(1).foo(2))
    assertEquals(8, pimpIt(2).foo(2, 4))
  }

  @Test def unboxAllTypesOfArguments_Issue899(): Unit = {
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
  class AnyValWithAnyRefPrimitiveMethods(private val x: Int) extends AnyVal {
    def eq(that: AnyRef): Boolean = (x + 1) == that
    def ne(that: AnyRef): Boolean = (x + 1) != that
    def synchronized[T](f: T): Any = f + "there"
  }
}
