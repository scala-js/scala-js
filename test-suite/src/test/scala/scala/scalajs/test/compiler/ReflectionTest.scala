/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package compiler

import scala.language.implicitConversions

import scala.scalajs.js

/** Tests the little reflection we support */
object ReflectionTest extends JasmineTest {

  describe("Scala.js Reflection (through java.lang.Class)") {
    it("should append $ to class name of objects") {
      expect(TestObject.getClass.getName).toEqual(
        "scala.scalajs.test.compiler.ReflectionTest$TestObject$")
    }

    it("should support isInstance") {
      class A
      class B extends A
      val b = new B
      expect(classOf[A].isInstance(b)).toBeTruthy
      expect(classOf[A].isInstance("hello")).toBeFalsy
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
  }

  object TestObject

}
