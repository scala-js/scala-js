/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.compiler

import scala.language.implicitConversions

import scala.scalajs.js

import org.scalajs.jasminetest.JasmineTest

/** Tests the little reflection we support */
object ReflectionTest extends JasmineTest {

  describe("Scala.js Reflection (through java.lang.Class)") {
    it("should append $ to class name of objects") {
      expect(TestObject.getClass.getName).toEqual(
        "scala.scalajs.testsuite.compiler.ReflectionTest$TestObject$")
    }

    it("should support isInstance") {
      class A
      class B extends A
      val b = new B
      expect(classOf[A].isInstance(b)).toBeTruthy
      expect(classOf[A].isInstance("hello")).toBeFalsy
    }

    it("getClass() for normal types") {
      class Foo {
        def bar() = super.getClass()
      }
      val foo = new Foo
      expect(foo.getClass() eq classOf[Foo]).toBeTruthy
      expect(foo.bar() eq classOf[Foo]).toBeTruthy
    }

    it("getClass() for anti-boxed primitive types") {
      implicit def classAsAny(c: java.lang.Class[_]): js.Any =
        c.asInstanceOf[js.Any]
      expect((false: Any).getClass).toBe(classOf[java.lang.Boolean])
      expect(('a': Any).getClass).toBe(classOf[java.lang.Character])
      expect((1.toByte: Any).getClass).toBe(classOf[java.lang.Integer])
      expect((1.toShort: Any).getClass).toBe(classOf[java.lang.Integer])
      expect((1: Any).getClass).toBe(classOf[java.lang.Integer])
      expect((1L: Any).getClass).toBe(classOf[java.lang.Long])
      expect((1.5f: Any).getClass).toBe(classOf[java.lang.Double])
      expect((1.5: Any).getClass).toBe(classOf[java.lang.Double])
      expect(((): Any).getClass).toBe(classOf[scala.runtime.BoxedUnit])
    }

    it("Class.isAssignableFrom should mimic runtime type tests behavior - #879") {
      expect(classOf[Short].isAssignableFrom(classOf[Byte])).toBeTruthy
      expect(classOf[Byte].isAssignableFrom(classOf[Byte])).toBeTruthy
      expect(classOf[Byte].isAssignableFrom(classOf[Short])).toBeFalsy
      expect(classOf[Int].isAssignableFrom(classOf[Byte])).toBeTruthy
      expect(classOf[Double].isAssignableFrom(classOf[Int])).toBeTruthy
      expect(classOf[Int].isAssignableFrom(classOf[Double])).toBeFalsy
      expect(classOf[Long].isAssignableFrom(classOf[Int])).toBeFalsy
    }
  }

  object TestObject

}
