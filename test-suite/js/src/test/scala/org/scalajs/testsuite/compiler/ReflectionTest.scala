/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import scala.language.implicitConversions

import scala.reflect.{classTag, ClassTag}

import scala.scalajs.js
import js.annotation.JSName

import org.scalajs.jasminetest.JasmineTest

/** Tests the little reflection we support */
object ReflectionTest extends JasmineTest {

  def implicitClassTagTest[A: ClassTag](x: Any): Boolean = x match {
    case x: A => true
    case _    => false
  }

  describe("Scala.js Reflection (through java.lang.Class)") {
    it("java.lang.Class.getName under normal circumstances") {
      expect(classOf[scala.Some[_]].getName).toEqual("scala.Some")
    }

    it("should append $ to class name of objects") {
      expect(TestObject.getClass.getName).toEqual(
        "org.scalajs.testsuite.compiler.ReflectionTest$TestObject$")
    }

    it("java.lang.Class.getName renamed through semantics") {
      expect(classOf[RenamedTestClass].getName).toEqual("renamed.test.Class")
    }

    it("should support isInstance") {
      class A
      class B extends A
      val b = new B
      expect(classOf[A].isInstance(b)).toBeTruthy
      expect(classOf[A].isInstance("hello")).toBeFalsy
    }

    it("isInstance for raw JS class") {
      js.Dynamic.global.ReflectionTestRawJSClass =
        js.eval("""(function() {})""")

      val obj = new ReflectionTestRawJSClass
      expect(obj.isInstanceOf[ReflectionTestRawJSClass]).toBeTruthy
      expect(classOf[ReflectionTestRawJSClass].isInstance(obj)).toBeTruthy

      val other = (5, 6): Any
      expect(other.isInstanceOf[ReflectionTestRawJSClass]).toBeFalsy
      expect(classOf[ReflectionTestRawJSClass].isInstance(other)).toBeFalsy

      val ct = classTag[ReflectionTestRawJSClass]
      expect(ct.unapply(obj).isDefined).toBeTruthy
      expect(ct.unapply(other).isDefined).toBeFalsy

      expect(implicitClassTagTest[ReflectionTestRawJSClass](obj)).toBeTruthy
      expect(implicitClassTagTest[ReflectionTestRawJSClass](other)).toBeFalsy
    }

    it("isInstance for raw JS traits should fail") {
      expect(() => classOf[ReflectionTestRawJSTrait].isInstance(5)).toThrow

      val ct = classTag[ReflectionTestRawJSTrait]
      expect(() => ct.unapply(new AnyRef)).toThrow

      expect(() => implicitClassTagTest[ReflectionTestRawJSTrait](new AnyRef)).toThrow
    }

    it("getClass() for normal types") {
      class Foo {
        def bar(): Class[_] = super.getClass()
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
      expect((1.toByte: Any).getClass).toBe(classOf[java.lang.Byte])
      expect((1.toShort: Any).getClass).toBe(classOf[java.lang.Byte])
      expect((1: Any).getClass).toBe(classOf[java.lang.Byte])
      expect((1L: Any).getClass).toBe(classOf[java.lang.Long])
      expect((1.5f: Any).getClass).toBe(classOf[java.lang.Float])
      expect((1.5: Any).getClass).toBe(classOf[java.lang.Float])
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

    it("getSuperclass - #1489") {
      expect(classOf[SomeChildClass].getSuperclass == classOf[SomeParentClass]).toBeTruthy
      expect(classOf[AnyRef].getSuperclass == null).toBeTruthy
      expect(classOf[String].getSuperclass == classOf[AnyRef]).toBeTruthy
      expect(classOf[Integer].getSuperclass == classOf[Number]).toBeTruthy

      expect(classOf[ChildClassWhoseDataIsAccessedDirectly].getSuperclass.getName).toEqual(
          "org.scalajs.testsuite.compiler.ReflectionTest$ParentClassWhoseDataIsNotAccessedDirectly")
    }

    it("cast(), positive") {
      expect(classOf[String].cast(null)).toBeNull
      expect(classOf[String].cast("hello")).toEqual("hello")
      expect(classOf[Seq[_]].cast(List(1, 2)) == List(1, 2)).toBeTruthy
      expect(() => classOf[Object].cast(js.Array(3, 4))).not.toThrow
    }

    when("compliant-asinstanceofs").
    it("cast(), negative") {
      expect(() => classOf[String].cast(5)).toThrow
      expect(() => classOf[Seq[_]].cast(Some("foo"))).toThrow
    }
  }

  object TestObject

  class RenamedTestClass

  @JSName("ReflectionTestRawJSClass")
  @js.native
  class ReflectionTestRawJSClass extends js.Object

  @js.native
  trait ReflectionTestRawJSTrait extends js.Object

  class SomeParentClass
  class SomeChildClass extends SomeParentClass

  class ParentClassWhoseDataIsNotAccessedDirectly
  class ChildClassWhoseDataIsAccessedDirectly extends ParentClassWhoseDataIsNotAccessedDirectly

}
